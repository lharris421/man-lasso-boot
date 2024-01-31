## Setup
source("./fig/setup/setup.R")

## Load Data
dataset <- "whoari"
## method <- "quantile"
## method <- "bucketfill"
dataset <- "Scheetz2006"
load(glue("{res_dir}/rds/lassoboot_comparison_{dataset}_{method}.rds"))


## Plotting function
plot_ci_comparison <- function(cis, lambdas, n, p, methods, nvars = 20) {

  ci_df <- do.call(rbind, cis) %>% data.frame()
  lambda_method <- methods[!is.null(lambdas)]
  lab <- paste0("N: ", n, ", p: ", p, ", ", paste0(paste(lambda_method, round(lambdas, 3), sep = ": "), collapse = ", "))

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
    mutate(method = method_pretty[method]) %>%
    ggplot() +
    geom_errorbar(aes(xmin = lower, xmax = upper, y = variable, color = method), alpha = .6) +
    geom_point(aes(x = estimate, y = variable, color = method), alpha = .6) +
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
    # xlim(c(-2, 2))

}

## Plotting
suppressMessages({
  pdf("./fig/lassoboot_comparison_data.pdf", width = 7.5)
  plot_ci_comparison(cis, lambdas, n, p, methods)
  dev.off()
  pobj <- plot_ci_comparison(cis, lambdas, n, p, methods)
  save(pobj, file = glue("{res_dir}/web/rds/lassoboot_comparison_{dataset}_{method}.rds"))
})
