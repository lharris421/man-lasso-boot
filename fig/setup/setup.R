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
  "grid", "glue", "lme4", "mgcv", "splines"
)

.libPaths(paste0(res_dir, "/local"))
lapply(packages, quietlyLoadPackage)
source(paste0(res_dir, "/R/saveR.R"))


colors <- palette()[c(2, 4, 3, 6, 7, 5)]
sec_colors <- c("black", "grey62")
background_colors <- c("#E2E2E2", "#F5F5F5")
#"black"   "#DF536B" "#61D04F" "#2297E6" "#28E2E5" "#CD0BBC" "#F5C710" "gray62"

ci_method <- "quantile"
methods_pretty <- c(
  "traditional" = "Traditional",
  "sample" = "Random Sample",
  "debiased" = "Debias",
  "acceptreject" = "Accept/Reject",
  "zerosample1" = "Zero Sample Single",
  "zerosample2" = "Zero Sample",
  "selective_inference" = "Selective Inference",
  "selectiveinference" = "Selective Inference",
  "blp" = "Bootstrap Lasso Projection",
  "fullconditional" = "Full Conditional",
  "truncatedzs2" = "Truncated Zero Sample",
  "zerosample2la" = "Lambda Adjusted"
)

rds_path <- glue("{res_dir}/rds/")
save_rds <- FALSE

nprod <- c(0.5, 1, 4)

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
  nom_data <- data.frame(alpha = alpha)

  plt <- ggplot() +
    geom_line(data = line_data, aes(x = x, y = y, color = n)) +
    geom_hline(data = line_data_avg, aes(yintercept = avg, color = n), linetype = 2) +
    geom_hline(data = nom_data, aes(yintercept = (1 - alpha)), linetype = 1, alpha = .5) +
    geom_text() +
    theme_bw() +
    xlab(expression(abs(beta))) +
    ylab(NULL) +
    coord_cartesian(ylim = c(0, 1), xlim = c(0, round(quantile(abs(per_var_data$truth), .98), 1))) +
    scale_color_manual(name = "N", values = colors)

  return(plt)
}

library(dplyr)
library(tidyr)

## Plotting function
plot_ci_comparison <- function(cis, nvars = 20) {

  plot_vars <- list()
  for (current_method in unique(cis$method)) {
    plot_vars[[current_method]] <- cis %>%
      filter(method == current_method) %>%
      dplyr::arrange(desc(abs(estimate))) %>%
      slice_head(n = nvars) %>%
      pull(variable)
  }
  plot_vars <- unique(unlist(plot_vars))

  plot_res <- cis %>%
    filter(variable %in% plot_vars) %>%
    dplyr::arrange(desc(abs(estimate)))

  plot_res$variable <- factor(plot_res$variable, levels = rev(plot_vars))

  plot_res %>%
    mutate(method = factor(methods_pretty[method], levels = methods_pretty[unique(cis$method)])) %>%
    ggplot() +
    geom_errorbar(aes(xmin = lower, xmax = upper, y = variable)) +
    geom_point(aes(x = estimate, y = variable)) +
    theme_bw() +
    ylab(NULL) + xlab(NULL) +
    scale_color_manual(name = "Method", values = colors) +
    theme(legend.position = "none",
          # legend.position = c(.9, .05),
          legend.justification = c("right", "bottom"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6),
          legend.background = element_rect(fill = "transparent")) +
    facet_wrap(~method)

}

plot_function <- function(plot_list) {

  lambda_max <- 1
  lambda_min <- 0.001
  lambda_seq <- 10^(seq(log(lambda_max, 10), log(lambda_min, 10), length.out = 10))

  cv_coverage <- plot_list %>%
    mutate(covered = truth >= lower & truth <= upper)  %>%
    filter(lambda == 11) %>%
    pull(covered) %>%
    mean()

  plot_list <- plot_list %>%
    mutate(covered = truth >= lower & truth <= upper)  %>%
    filter(lambda <= 10)

  overall_cov <- plot_list %>%
    dplyr::mutate(lambda = lambda_seq[lambda]) %>%
    dplyr::group_by(lambda) %>%
    dplyr::summarise(coverage = mean(covered)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(group = "Overall")
  size_cov <- plot_list %>%
    dplyr::mutate(lambda = lambda_seq[lambda]) %>%
    dplyr::mutate(group = as.character(group)) %>%
    dplyr::group_by(lambda, group) %>%
    dplyr::summarise(coverage = mean(covered)) %>%
    dplyr::ungroup()
  coverage_data <- dplyr::bind_rows(overall_cov, size_cov) %>%
    dplyr::mutate(group = factor(group, levels = c("Small", "Moderate", "Large", "Overall")))

  gg <- ggplot(data = coverage_data, aes(x = lambda, y = coverage, group = group, color = group)) +
    geom_line() +
    # geom_vline(xintercept = plot_list[[2]], linetype = "dashed", color = sec_colors[1], linewidth = .5) +
    geom_hline(yintercept = 0.8, linewidth = .5) +
    theme_bw() +
    scale_x_continuous(trans = log10_trans(),
                       breaks = trans_breaks('log10', function(x) 10^x),
                       labels = trans_format('log10', math_format(10^.x))) +
    coord_cartesian(xlim = c(1, .001), ylim = c(0, 1.0)) +
    scale_color_manual(name = expression(abs(beta)), values = colors) +
    ggtitle(glue("{plot_list$dist_type}"), subtitle = glue("CV Coverage: {cv_coverage}"))

  return(gg)

}

dlaplace <- function(x, rate = 1) {
  dexp(abs(x), rate) / 2
}

