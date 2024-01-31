## Setup
source("./fig/setup/setup.R")

## Load Data
# method <- "bucketfill"
load(glue("{res_dir}/rds/lassoboot_comparison_laplace_100_{method}.rds"))
cutoff <- 3
xs <- seq(0, cutoff, by = .01)
methods <- unique(all_info[[1]]$method)

plots <- list()
for (j in 1:3) {
  curr_info <- all_info[[j]]

  # ref <- curr_info %>%
  #   data.frame() %>%
  #   filter(method == "mode") %>%
  #   mutate(baseline_width = upper - lower)

  curr_info <- all_info[[j]]
  plot_res <- list()

  for (i in 1:length(methods)) {
    plot_data <- curr_info %>%
      data.frame() %>%
      filter(method == methods[i]) %>%
      mutate(width = upper - lower) %>%
      mutate(mag_truth = abs(truth))
    print(methods[i])
    # mean(plot_data$width_diff)
    fit <- gam(width ~ s(mag_truth) + s(group, bs = "re"), data = plot_data)
    ys <- predict(fit, data.frame(mag_truth = xs, group = 101), type ="response")
    # fit <- lmer(width_diff ~ ns(mag_truth, df = 4) + (1|group), data = plot_data)
    # ys <- predict(fit, data.frame(mag_truth = xs, group = 101), type ="response", allow.new.levels = TRUE)
    plot_res[[i]] <- data.frame(xs = xs, width = ys, method = method_pretty[methods[i]])
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
  # case_when(
  #   sign(estimate) == -1 ~ mean(c(lower, upper)) - estimate,
  #   sign(estimate) == 0 ~ -1*abs(mean(c(lower, upper))),
  #   sign(estimate) == 1 ~ estimate - mean(c(lower, upper))
  # )
  case_when(
    sign(estimate) == -1 ~ mean(c(lower, upper)) - truth,
    sign(estimate) == 0 ~ -1*abs(mean(c(lower, upper))),
    sign(estimate) == 1 ~ truth - mean(c(lower, upper))
  )
}

# bias_est(-3, -2, -1)
# bias_est(0, 0, 2)
# bias_est(1, 0, 2)
# bias_est(1, 0, 3)

plots_bias <- list()
for (j in 1:3) {
  curr_info <- all_info[[j]]

  # ref <- curr_info %>%
  #   data.frame() %>%
  #   filter(method == "mode") %>%
  #   rowwise() %>%
  #   mutate(baseline_bias = bias_est(estimate, lower, upper, truth))
  xs <- seq(0, cutoff, by = .01)
  plot_res <- list()
  for (i in 1:length(methods)) {
    samp <- curr_info %>%
      data.frame() %>%
      filter(method == methods[i]) %>%
      rowwise() %>%
      mutate(bias = bias_est(estimate, lower, upper, truth))
    # plot_data <- ref %>%
    #   inner_join(samp, by = c("group", "truth")) %>%
    #   mutate(mag_truth = abs(truth))
    plot_data <- samp %>%
      mutate(mag_truth = abs(truth))
    print(methods[i])
    # print(mean(plot_data$bias))
    fit <- gam(bias ~ s(mag_truth) + s(group, bs = "re"), data = plot_data)
    ys <- predict(fit, data.frame(mag_truth = xs, group = 101), type ="response")
    # fit <- lmer(bias_diff ~ ns(mag_truth, df = 4) + (1|group), data = plot_data)
    # ys <- predict(fit, data.frame(mag_truth = xs, group = 101), type ="response", allow.new.levels = TRUE)
    plot_res[[i]] <- data.frame(xs = xs, bias = ys, method = method_pretty[methods[i]])
  }

  plots_bias[[j]] <- do.call(rbind, plot_res) %>%
    ggplot(aes(x = xs, y = bias, color = method)) +
    geom_line() +
    theme_bw() +
    xlab(NULL) +
    ylab(NULL) +
    # annotate("text", x = 0.1, y = 0.1, label = paste0("N = ", ns[j]), size = 5) +
    # coord_cartesian(ylim = c(0, 1)) +
    scale_color_manual(name = "Method", values = colors)
}

plots[[1]] <- plots[[1]] +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

plots[[2]] <- plots[[2]] +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

plots[[3]] <- plots[[3]] +
  theme(legend.position = "none")

plots_bias[[1]] <- plots_bias[[1]] +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

plots_bias[[2]] <- plots_bias[[2]] +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

plots_bias[[3]] <- plots_bias[[3]] +
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
  pdf("./fig/lassoboot_comparison_laplace_tradref.pdf", height = 6.5)
  grid.arrange(grobs = list(plots[[1]], plots_bias[[1]], plots[[2]], plots_bias[[2]], plots[[3]], plots_bias[[3]]), nrow = 3, ncol = 2, left = left_label, right = right_label)
  dev.off()
  gobj <- grid.arrange(grobs = list(plots[[1]], plots_bias[[1]], plots[[2]], plots_bias[[2]], plots[[3]], plots_bias[[3]]), nrow = 3, ncol = 2, left = left_label, right = right_label, bottom = bottom_label)
  save(gobj, file = glue("{res_dir}/web/rds/lassoboot_comparison_laplace_tradref_{method}.rds"))
})
