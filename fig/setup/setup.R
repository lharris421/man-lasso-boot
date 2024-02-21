rm(list=ls())
unloadNamespace("ncvreg")
res_dir <- switch(Sys.info()['user'],
                  'pbreheny' = '~/res/lasso-boot',
                  'loganharris' = '../lasso-boot')

.libPaths(paste0(res_dir, "/local"))
quietlyLoadPackage <- function(package) {
  suppressPackageStartupMessages(library(package, character.only = TRUE))
}

packages <- c(
  "dplyr", "tidyr", "ggplot2", "ncvreg", "gridExtra", "scales", "kableExtra",
  "grid", "glue", "lme4", "gam", "mgcv", "splines"
)

.libPaths(paste0(res_dir, "/local"))
lapply(packages, quietlyLoadPackage)
colors <- palette()[c(2, 4, 3, 6, 7, 5)]
sec_colors <- c("black", "grey62")
background_colors <- c("#E2E2E2", "#F5F5F5")

ci_method <- "quantile"
methods_pretty <- c(
  "traditional" = "Traditional",
  "sample" = "Random Sample",
  "debiased" = "Debias",
  "acceptreject" = "Accept/Reject",
  "zerosample1" = "Zero Sample Single",
  "zerosample2" = "Zero Sample",
  "selective_inference" = "Selective Inference",
  "blp" = "Bootstrap Lasso Projection",
  "fullconditional" = "Full Conditional",
  "truncatedzs2" = "Truncated Zero Sample"
)

save_rds <- FALSE

single_method_plot <- function(per_var_data, ns, alpha) {
  cutoff <- round(quantile(abs(per_var_data$truth), .98), 1)
  line_data <- list()
  line_data_avg <- list()
  for (j in 1:length(ns)) {

    tmp <- per_var_data %>%
      mutate(
        covered = lower <= truth & upper >= truth,
        mag_truth = abs(truth), covered = as.numeric(covered)
      ) %>%
      filter(n == ns[j])

    fit <- gam(covered ~ s(mag_truth) + s(group, bs = "re"), data = tmp, family = binomial)
    xs <- seq(0, cutoff, by = .01)
    ys <- predict(fit, data.frame(mag_truth = xs, group = 101), type ="response")

    line_data[[j]] <- data.frame(x = xs, y = ys, n = factor(ns[j]))
    line_data_avg[[j]] <- data.frame(avg = mean(tmp$covered), n = factor(ns[j]))
  }

  line_data_avg <- do.call(rbind, line_data_avg)
  line_data <- do.call(rbind, line_data)

  print(alpha)
  plt <- ggplot() +
    geom_line(data = line_data, aes(x = x, y = y, color = n)) +
    geom_hline(data = line_data_avg, aes(yintercept = avg, color = n), linetype = 2) +
    geom_hline(aes(yintercept = (1 - alpha)), linetype = 1, alpha = .5) +
    geom_text() +
    theme_bw() +
    xlab(expression(abs(beta))) +
    ylab(NULL) +
    coord_cartesian(ylim = c(0, 1), xlim = c(0, round(quantile(abs(per_var_data$truth), .98), 1))) +
    scale_color_manual(name = "N", values = colors)
  print(1 - alpha)

  return(plt)
}
