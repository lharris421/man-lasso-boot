## Setup
source("./fig/setup/setup.R")

methods <- c("selectiveinference", "lasso", "blp")
ns <- c(100)
data_type <- "laplace"
SNR <- 1
alpha <- .2
p <- 100
lambda <- "cv"

params_grid <- expand.grid(list(data = data_type, n = ns, snr = SNR, lambda = lambda,
                                method = methods,
                                nominal_coverage = (1 - alpha) * 100, p = p))

params_grid <- cbind(params_grid, alpha = c(NA, 1, NA))

# Fetching and combining data
per_var_data <- list()
for (i in 1:nrow(params_grid)) {
  res_list <- read_objects(rds_path, params_grid[i,])
  per_var_data[[i]] <- res_list$per_var_n
  if (params_grid$method[i] == "lasso") {
    per_var_data[[i]] <-  per_var_data[[i]] %>%
      filter(submethod %in% c("hybrid"))
  }
  if (params_grid$method[i] %in% c("blp", "selectiveinference")) {
    per_var_data[[i]] <-  per_var_data[[i]] %>%
      rename(lower = estimate, upper = lower, estimate = upper)
  }
}
model_res <- do.call(rbind, per_var_data) %>%
  data.frame() %>%
  mutate(
    covered = lower <= truth & upper >= truth,
    mag_truth = abs(truth), covered = as.numeric(covered)
  )

n_methods <- length(unique(model_res$submethod))
methods <- unique(model_res$submethod)

cutoff <- .275
line_data <- list()
line_data_avg <- list()
for (i in 1:length(methods)) {
  print(methods[i])
  tmp <- model_res %>%
    mutate(method = stringr::str_remove(submethod, "_")) %>%
    filter(method == methods[i] & n == ns)

  if (i == 1) {
    xvals <- seq(from = 0, to = cutoff, length.out = cutoff * 100 + 1)
    density_data <- data.frame(x = xvals, density = 2 * dlaplace(xvals, rate = 14.14))
  }

  grp_succ <- tmp %>%
    filter(!is.na(estimate)) %>%
    pull(group) %>%
    unique()

  tmp %>%
    filter(!is.na(estimate)) %>%
    group_by(n) %>%
    summarise(
      perc_succ = length(unique(group))
    ) %>%
    left_join(
      tmp %>%
        filter(group %in% grp_succ) %>%
        group_by(n) %>%
        summarise(
          perc_incl = mean(!is.na(estimate))
        )) %>%
    print()

  tmp <- tmp %>%
    filter(!is.na(estimate)) %>%
    mutate(covered = lower <= truth & upper >= truth)

  # fit <- gam(covered ~ s(mag_truth) + s(group, bs = "re"), data = tmp, family = binomial)
  fit <- gam(covered ~ s(mag_truth), data = tmp, family = binomial)
  xs <- seq(0, cutoff, by = .01)
  ys <- predict(fit, data.frame(mag_truth = xs, group = 101), type ="response")
  line_data[[i]] <- data.frame(x = xs, y = ys, method = methods[i])

  line_data_avg[[i]] <- data.frame(avg = mean(tmp$covered), method = methods_pretty[methods[i]])

}

line_data_avg <- do.call(rbind, line_data_avg)
line_data <- do.call(rbind, line_data)


plt <- ggplot() +
  geom_line(data = line_data %>% mutate(method = methods_pretty[method]), aes(x = x, y = y, color = method)) +
  geom_hline(data = line_data_avg, aes(yintercept = avg, color = method), linetype = 2) +
  geom_hline(aes(yintercept = .8), linetype = 1) +
  geom_area(data = density_data, aes(x = x, y = density / max(density)), fill = "grey", alpha = 0.5) +
  theme_bw() +
  xlab(expression(abs(beta))) +
  ylab("Coverage") +
  coord_cartesian(ylim = c(0, 1)) +
  scale_color_manual(name = "Method", values = colors)


pdf("./fig/laplace_comparison.pdf", height = 4, width = 8)
plt
dev.off()

