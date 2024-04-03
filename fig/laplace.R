## Setup
source("./fig/setup/setup.R")

plots <- list()

methods <- c("sample", "zerosample2", "debiased", "traditional")
n_values <- c(50, 100, 400) # ns values you are interested in
data_type <- "laplace"
rate <- 2
SNR <- 1
corr <- "exchangeable"
rho <- 0
alpha <- .2
p <- 100
modifier <- NA

new_folder <- "/Users/loganharris/github/lasso-boot/new_rds/"

params_grid <- expand.grid(list(data = data_type, n = n_values, rate = rate, snr = SNR,
                    correlation_structure = corr, correlation = rho, method = methods, lambda = "cv",
                    ci_method = "quantile", nominal_coverage = alpha * 100, p = p, modifier = modifier))


# Fetching and combining data
per_var_data <- list()
for (i in 1:nrow(params_grid)) {
  read_objects(rds_path, params_grid[i,])
  print(unique(per_var_n$n))
  per_var_data[[i]] <- per_var_n %>%
    mutate(method = ifelse(params_grid[i,"ci_method"] == "mvn_uni", "debiased_normalized", method),
           method = ifelse(params_grid[i,"ci_method"] == "mvn_corrected", "debiased_corrected", method))
}
per_var_data <- do.call(rbind, per_var_data) %>%
  data.frame()


cutoff <- 2
ns <- unique(per_var_data$n)
for (j in 1:length(ns)) {

  model_res <- per_var_data %>%
    mutate(
      covered = lower <= truth & upper >= truth,
      mag_truth = abs(truth), covered = as.numeric(covered)
    )

  line_data <- list()
  line_data_avg <- list()
  for (i in 1:length(methods)) {
    print(methods[i])
    tmp <- model_res %>%
      filter(method == methods[i] & n == ns[j])

    if (i == 1) {
      xvals <- seq(from = 0, to = cutoff, length.out = cutoff * 100 + 1)
      density_data <- data.frame(x = xvals, density = 2 * dlaplace(xvals, rate = 2))
    }

    tmp <- tmp %>%
      filter(!is.na(estimate))

    print(mean(tmp$covered))
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
    geom_hline(aes(yintercept = 1 - alpha), linetype = 1, alpha = .5) +
    geom_area(data = density_data, aes(x = x, y = density / max(density)), fill = "grey", alpha = 0.5) +
    theme_bw() +
    xlab(expression(abs(beta))) +
    ylab(NULL) +
    annotate("text", x = 0.1, y = 0.1, label = paste0("N = ", ns[j]), size = 5) +
    coord_cartesian(ylim = c(0, 1)) +
    scale_color_manual(name = "Method", values = colors)


}

plots[[2]] <- plots[[2]] +
  theme(legend.position = c(.95, .05),
        legend.justification = c("right", "bottom"),
        legend.direction = "horizontal",
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.background = element_rect(fill = "transparent"))


p1 <- plots[[1]]
p2 <- plots[[2]]
p3 <- plots[[3]]

pdf("./fig/laplace.pdf", height = 3.5)
p2
dev.off()


