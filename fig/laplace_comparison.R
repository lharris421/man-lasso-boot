## Setup
source("./fig/setup/setup.R")

plots <- list()

methods <- c("selectiveinference", "zerosample2", "blp")
ns <- c(100)
data_type <- "laplace"
rate <- 2
SNR <- 1
corr <- "exchangeable"
rho <- 0
alpha <- .2
p <- 100
modifier <- NA
lambda <- "cv"

params_grid <- expand.grid(list(data = data_type, n = ns, rate = rate, snr = SNR, lambda = "cv",
                                correlation_structure = corr, correlation = rho, method = methods,
                                ci_method = "quantile", nominal_coverage = alpha * 100, p = p, modifier = modifier))
# Fetching and combining data
per_var_data <- list()
for (i in 1:nrow(params_grid)) {
  read_objects(rds_path, params_grid[i,])
  per_var_data[[i]] <- per_var_n
}
model_res <- do.call(rbind, per_var_data) %>%
  data.frame() %>%
  mutate(
    covered = lower <= truth & upper >= truth,
    mag_truth = abs(truth), covered = as.numeric(covered)
  )

n_methods <- length(methods)

cutoff <- 3
for (j in 1:length(ns)) {
  line_data <- list()
  line_data_avg <- list()
  for (i in 1:length(methods)) {
    print(methods[i])
    tmp <- model_res %>%
      mutate(method = stringr::str_remove(method, "_")) %>%
      filter(method == methods[i] & n == ns[j])

    if (i == 1) {
      xvals <- seq(from = 0, to = cutoff, length.out = cutoff * 100 + 1)
      density_data <- data.frame(x = xvals, density = 2 * dlaplace(xvals, rate = 2))
    }

    tmp %>%
      filter(!is.na(estimate)) %>%
      group_by(n) %>%
      summarise(
        perc_succ = length(unique(group))
      ) %>%
      left_join(
        tmp %>%
          group_by(n) %>%
          summarise(
            perc_incl = mean(!is.na(estimate))
          )) %>%
      print()

    tmp <- tmp %>%
      filter(!is.na(estimate))

    fit <- gam(covered ~ s(mag_truth) + s(group, bs = "re"), data = tmp, family = binomial)
    xs <- seq(0, cutoff, by = .01)
    ys <- predict(fit, data.frame(mag_truth = xs, group = 101), type ="response")
    line_data[[i]] <- data.frame(x = xs, y = ys, method = methods[i])

    line_data_avg[[i]] <- data.frame(avg = mean(tmp$covered), method = methods_pretty[methods[i]])

  }

  line_data_avg <- do.call(rbind, line_data_avg)
  line_data <- do.call(rbind, line_data)


  plots[[j]] <- ggplot() +
    geom_line(data = line_data %>% mutate(method = methods_pretty[method]), aes(x = x, y = y, color = method)) +
    geom_hline(data = line_data_avg, aes(yintercept = avg, color = method), linetype = 2) +
    geom_hline(aes(yintercept = .8), linetype = 1) +
    geom_area(data = density_data, aes(x = x, y = density / max(density)), fill = "grey", alpha = 0.5) +
    theme_bw() +
    xlab(expression(abs(beta))) +
    ylab(NULL) +
    # annotate("text", x = 0.1, y = 0.1, label = paste0("N = ", ns[j]), size = 5) +
    coord_cartesian(ylim = c(0, 1)) +
    scale_color_manual(name = "Method", values = colors)


}

plots[[1]] <- plots[[1]] +
  theme(legend.position = c(.95, .05),
        legend.justification = c("right", "bottom"),
        legend.direction = "horizontal",
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.background = element_rect(fill = "transparent"))


pdf("./fig/laplace_comparison.pdf", height = 4, width = 6)
plots[[1]]
dev.off()

