## Setup
source("./fig/setup/setup.R")

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
    scale_color_manual(name = "Method", values = colors) +
    ylab(NULL) + xlab(NULL) +
    coord_cartesian(xlim=c(-3, 3)) +
    annotate("text", x = -2.9, y = 1.5, label = paste0("N = ", n), size = 5)

}

## Load Data
load(glue("{res_dir}/rds/method_comparison_laplace_{quantiles}_{method}.rds"))

## Plotting
plots <- lapply(plot_res, plot_ci_comparison)
plots[[1]] <- plots[[1]] +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = c(.95, .05),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.background = element_rect(fill = "transparent"))
plots[[2]] <- plots[[2]] +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")
plots[[3]] <- plots[[3]] +
  theme(legend.position = "none")

suppressMessages({
  pdf("./fig/method_comparison_laplace.pdf", height = 8, width = 7)
  grid.arrange(grobs = plots, ncol = 1)
  dev.off()
  png("./fig/method_comparison_laplace.png", width = 1000, height = 1100)
  grid.arrange(grobs = plots, ncol = 1)
  dev.off()
})



