## Setup
source("./fig/setup/setup.R")

## Load Data
method <- "bucketfill"
load(glue("{res_dir}/rds/method_comparison_whoari_{method}.rds"))

## Plotting function
plot_ci_comparison <- function(ci_list, nvars = 40) {

  ci_df <- do.call(rbind, ci_list[1:3]) %>% data.frame()
  lambdas <- unlist(ci_list[4:6])
  lambda_method <- c("Selective Inference", "Bootstrap Lasso Projection", "Lasso Bootstrap")[!unlist(lapply(ci_list[4:6], is.null))]
  n <- ci_list[[7]]
  p <- ci_list[[8]]
  lab <- paste0("N: ", n, ", p: ", round(p, 3), ", ", paste0(paste(lambda_method, round(lambdas, 3), sep = ": "), collapse = ", "))

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
    ylab(NULL) + xlab(NULL) +
    scale_color_manual(name = "Method", values = colors) +
    coord_cartesian(xlim=c(-.6, .6)) +
    theme(legend.position = c(.97, .00),
          legend.justification = c("right", "bottom"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6),
          legend.background = element_rect(fill = "transparent"))

}

## Plotting
plots <- plot_ci_comparison(plot_res)

suppressMessages({
  pdf("./fig/method_comparison_whoari.pdf", width = 10, height = 5)
  plot_ci_comparison(plot_res)
  dev.off()
  png("./fig/method_comparison_whoari.png", width = 1000, height = 500)
  plot_ci_comparison(plot_res)
  dev.off()
})
