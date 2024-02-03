## Setup
source("./fig/setup/setup.R")

## Load Data
n <- 50
p <- 25
method <- "quantile"
methods <- c("ridge", "zerosample2")
methods_pretty <- c(methods_pretty, "ridge" = "Ridge")

cis <- list()
examples <- list()
for (i in 1:length(methods)) {
  load(glue("{res_dir}/rds/highcorr_{methods[i]}.rds"))
  cis[[i]] <- confidence_interval
  examples[[i]] <- example
}
names(cis) <- methods
names(examples) <- methods


plot_ridge <- function(ridge_ci, n = 30, quiet = TRUE) {

  plot_res <- ridge_ci[-1,] %>%
    data.frame()

  plot_res$variable <- rownames(plot_res)
  plot_res <- plot_res %>%
    dplyr::arrange(desc(abs(estimate))) %>%
    head(n)

  plot_res$variable <- factor(plot_res$variable, levels = rev(plot_res$variable))

  plot_res %>%
    ggplot() +
    geom_errorbar(aes(xmin = lower, xmax = upper, y = variable)) +
    geom_point(aes(x = estimate, y = variable)) +
    theme_bw() +
    labs(y = "Variable", x = "Estimate")

}

plots <- list()
eplots <- list()
for (i in 1:length(methods)) {
  curr_method <- methods[i]
  current_cis <- do.call(rbind, cis[[curr_method]]) %>%
    data.frame()
  current_example <- examples[[curr_method]]

  if (curr_method == "ridge") {
    variables <- as.vector(sapply(cis[[curr_method]], function(x) rownames(x)))
    pdat <- current_cis %>%
      mutate(variable = variables) %>%
      filter(variable %in% c("A1", "B1", "N1")) %>%
      pivot_longer(Lower:Upper, names_to = "bound", values_to = "value") %>%
      mutate(value = as.numeric(value))
  } else {
    pdat <- current_cis %>%
      filter(variable %in% c("A1", "B1", "N1")) %>%
      pivot_longer(lower:upper, names_to = "bound", values_to = "value")
  }


 plots[[1 + 2*(i-1)]] <- pdat %>%
    ggplot() +
    geom_boxplot(aes(x = value, y = variable, color = bound)) +
    theme_bw() +
    coord_cartesian(xlim = c(-1, 2)) +
    theme(
      legend.position = "none"
      #plot.background = element_rect(fill=background_colors[2])
    ) +
    ylab(NULL) +
    xlab(NULL) +
    scale_color_manual(values = colors) +
    annotate("text", x = 1.2, y = 3.4, label = methods_pretty[methods[i]], size = 5)

 if (methods[i] == "ridge") {
   ridge_example <- current_example
   colnames(ridge_example) <- tolower(colnames(ridge_example))
   tmp_plot <- plot_ridge(ridge_example)
 } else {
   tmp_plot <- plot(current_example, method = method)
 }
 plots[[2 + 2*(i-1)]] <- tmp_plot +
   coord_cartesian(xlim = c(-1, 2)) +
   ylab(NULL) +
   xlab(NULL)

}


left_label <- textGrob("Variable", gp = gpar(fontsize = 12), rot = 90)
bottom_label <- textGrob("Interval Endpoint", gp = gpar(fontsize = 12))

suppressMessages({
  pdf("./fig/highcorr.pdf", height = length(methods) * 3)
  grid.arrange(grobs = plots, ncol = 2, left = left_label, bottom = bottom_label)
  dev.off()
  gobj <- grid.arrange(grobs = plots, ncol = 2, left = left_label, bottom = bottom_label)
  save(gobj, file = glue("{res_dir}/web/rds/highcorr.rds"))
})

