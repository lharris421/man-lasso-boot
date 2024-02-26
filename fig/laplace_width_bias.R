## Setup
source("./fig/setup/setup.R")

## Load Data
cutoff <- 2
xs <- seq(0, cutoff, by = .01)


methods <- c("traditional", "sample", "debiased", "zerosample2"); n_methods <- length(methods)
ns <- c(50, 100, 400) # ns values you are interested in
data_type <- "laplace"
rate <- 2
SNR <- 1
corr <- "exchangeable"
rho <- 0
alpha <- .2
p <- 100
modifier <- NA
lambda <- "cv"

params_grid <- expand.grid(list(data = data_type, n = ns, rate = rate, snr = SNR, lambda = lambda,
                                correlation_structure = corr, correlation = rho, method = methods,
                                ci_method = "quantile", nominal_coverage = alpha * 100, p = p, modifier = modifier))
# Fetching and combining data
per_var_data <- list()
for (i in 1:nrow(params_grid)) {
  read_objects(rds_path, params_grid[i,])
  per_var_data[[i]] <- per_var_n
}
per_var_data <- do.call(rbind, per_var_data) %>%
  data.frame() %>%
  mutate(method = stringr::str_remove(method, "_"))


plots <- list()
for (j in 1:length(ns)) {

  plot_res <- list()

  for (i in 1:length(methods)) {
    plot_data <- per_var_data %>%
      filter(method == methods[i] & n == ns[j]) %>%
      mutate(width = upper - lower) %>%
      filter(!is.na(width) & is.finite(width)) %>%
      mutate(mag_truth = abs(truth))
    print(methods[i])
    fit <- gam(width ~ s(mag_truth) + s(group, bs = "re"), data = plot_data)
    ys <- predict(fit, data.frame(mag_truth = xs, group = 101), type ="response")
    plot_res[[i]] <- data.frame(xs = xs, width = ys, method = methods_pretty[methods[i]])
  }

  plots[[j]] <- do.call(rbind, plot_res) %>%
    ggplot(aes(x = xs, y = width, color = method)) +
    geom_line() +
    theme_bw() +
    xlab(NULL) +
    ylab(NULL) +
    # annotate("text", x = 0.1, y = 0.1, label = paste0("N = ", ns[j]), size = 5) +
    # coord_cartesian(ylim = c(0, 1)) +
    scale_color_manual(name = "Method", values = colors)
}


## Center
## Need to update this when each method is running under same random seed
bias_est <- function(estimate, lower, upper, truth) {
  case_when(
    sign(truth) == -1 ~ mean(c(lower, upper)) - truth,
    # sign(estimate) == 0 ~ -1*abs(mean(c(lower, upper))),
    sign(truth) == 1 ~ truth - mean(c(lower, upper))
  )
}

plots_bias <- list()
for (j in 1:length(ns)) {

  xs <- seq(0, cutoff, by = .01)
  plot_res <- list()
  for (i in 1:length(methods)) {

    plot_data <- per_var_data %>%
      filter(method == methods[i] & n == ns[j]) %>%
      rowwise() %>%
      mutate(bias = bias_est(estimate, lower, upper, truth)) %>%
      filter(!is.na(bias) & is.finite(bias)) %>%
      mutate(mag_truth = abs(truth))

    print(methods[i])
    fit <- gam(bias ~ s(mag_truth) + s(group, bs = "re"), data = plot_data)
    ys <- predict(fit, data.frame(mag_truth = xs, group = 101), type ="response")
    plot_res[[i]] <- data.frame(xs = xs, bias = ys, method = methods_pretty[methods[i]])
  }

  plots_bias[[j]] <- do.call(rbind, plot_res) %>%
    ggplot(aes(x = xs, y = bias, color = method)) +
    geom_line() +
    theme_bw() +
    xlab(NULL) +
    ylab(NULL) +
    scale_color_manual(name = "Method", values = colors)
}

# plots[[1]] <- plots[[1]] +
#   theme(legend.position = "none",
#         axis.title.x = element_blank(),
#         axis.text.x = element_blank(),
#         axis.ticks.x = element_blank())
#
# plots[[2]] <- plots[[2]] +
#   theme(legend.position = "none",
#         axis.title.x = element_blank(),
#         axis.text.x = element_blank(),
#         axis.ticks.x = element_blank())
#
# plots[[3]] <- plots[[3]] +
#   theme(legend.position = "none")
#
# plots_bias[[1]] <- plots_bias[[1]] +
#   theme(legend.position = "none",
#         axis.title.x = element_blank(),
#         axis.text.x = element_blank(),
#         axis.ticks.x = element_blank())
#
# plots_bias[[2]] <- plots_bias[[2]] +
#   theme(legend.position = "none",
#         axis.title.x = element_blank(),
#         axis.text.x = element_blank(),
#         axis.ticks.x = element_blank())
#
# plots_bias[[3]] <- plots_bias[[3]] +
#   theme(legend.position = c(0.5, 0.1),
#                   legend.direction = "horizontal",
#                   legend.background = element_rect(fill = NA),
#                   legend.title = element_text(size = 8),
#                   legend.text = element_text(size = 6),
#                   legend.key.width = unit(0.3, "cm"),
#                   legend.key.height = unit(0.2, "cm"))


plots[[2]] <- plots[[2]] +
  theme(legend.position = "none")

plots_bias[[2]] <- plots_bias[[2]] +
  theme(legend.position = c(0.5, 0.1),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = NA),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        legend.key.width = unit(0.3, "cm"),
        legend.key.height = unit(0.2, "cm"))

left_label <- textGrob("Width", gp = gpar(fontsize = 12), rot = 90)
right_label <- textGrob("Central Bias", gp = gpar(fontsize = 12), rot = 270)
bottom_label <- textGrob(expression(abs(beta)), gp = gpar(fontsize = 12))

suppressMessages({
  # pdf("./fig/laplace_width_bias.pdf", width = 7.5)
  # grid.arrange(grobs = list(plots[[1]], plots_bias[[1]], plots[[2]], plots_bias[[2]], plots[[3]], plots_bias[[3]]), nrow = 3, ncol = 2, left = left_label, right = right_label, bottom = bottom_label)
  pdf("./fig/laplace_width_bias.pdf", height = 3.5)
  grid.arrange(grobs = list(plots[[2]], plots_bias[[2]]), nrow = 1, ncol = 2, left = left_label, right = right_label, bottom = bottom_label)
  dev.off()
  if (save_rds) {
    gobj <- grid.arrange(grobs = list(plots[[1]], plots_bias[[1]], plots[[2]], plots_bias[[2]], plots[[3]], plots_bias[[3]]), nrow = 3, ncol = 2, left = left_label, right = right_label, bottom = bottom_label)
    save(gobj, file = glue("{res_dir}/web/rds/laplace_width_bias.rds"))
  }
})
