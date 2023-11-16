res_dir <- switch(Sys.info()['user'],
                  'pbreheny' = '~/res/lasso-boot',
                  'loganharris' = '../lasso-boot')

quietlyLoadPackage <- function(package) {
  suppressPackageStartupMessages(library(package, character.only = TRUE))
}

packages <- c("dplyr", "ggplot2", "ncvreg", "gridExtra", "scales")

.libPaths(paste0(res_dir, "/local"))
lapply(packages, quietlyLoadPackage)

## Plotting function
plot_ci_comparison <- function(ci_list, nvars = 60) {

  ci_df <- do.call(rbind, ci_list[1:3]) %>% data.frame()
  lambdas <- unlist(ci_list[4:6])
  lambda_method <- c("Selective Inference", "Bootstrap Lasso Projection", "Lasso Bootstrap")[!unlist(lapply(ci_list[4:6], is.null))]
  n <- ci_list[[7]]
  true_lambda <- ci_list[[8]]
  lab <- paste0("N: ", n, ", True Lambda: ", round(true_lambda, 3), ", ", paste0(paste(lambda_method, round(lambdas, 3), sep = ": "), collapse = ", "))

  plot_vars <- list()
  for (current_method in unique(ci_df$method)) {
    plot_vars[[current_method]] <- ci_df %>%
      filter(method == current_method) %>%
      dplyr::arrange(desc(abs(estimate))) %>%
      slice_head(n = nvars) %>%
      pull(variable)
  }
  plot_vars <- unique(unlist(plot_vars))

  plot_res <- ci_df %>%
    filter(variable %in% plot_vars) %>%
    dplyr::arrange(desc(abs(estimate)))

  plot_res$variable <- factor(plot_res$variable, levels = rev(plot_vars))

  plot_res %>%
    ggplot() +
    geom_errorbar(aes(xmin = lower, xmax = upper, y = variable, color = method), alpha = .6) +
    geom_point(aes(x = estimate, y = variable, color = method), alpha = .6) +
    theme_bw() +
    labs(y = "Variable", x = "Estimate") +
    ggtitle(lab) +
    coord_cartesian(xlim=c(-3, 3))

}

## Load Data
load(paste0(res_dir, "/rds/method_comparison_sim.rds"))

## Plotting
plots <- lapply(plot_res, plot_ci_comparison)

pdf("./fig/method_comparison_sim.pdf", width = 10, height = 11)
suppressMessages({
  grid.arrange(grobs = plots, ncol = 1)
})

dev.off()
