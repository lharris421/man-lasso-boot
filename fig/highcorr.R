## Setup
source("./fig/setup/setup.R")

## Load Data
data_type <- "abn"
alpha <- .2
modifier <- character()
modifier[1] <- NA
arg_list <- list(data = data_type,
                 n = 100,
                 p = 100,
                 snr = 1,
                 sd = ifelse(data_type == "normal", sd, NA),
                 rate = ifelse(data_type == "laplace", rt, NA),
                 a = ifelse(data_type == "abn", 1, NA),
                 b = ifelse(data_type == "abn", 1, NA),
                 correlation_structure = "exchangeable",
                 correlation = 0.99,
                 correlation_noise = ifelse(data_type == "abn", 0, NA),
                 method = c("ridge"),
                 ci_method = "quantile",
                 nominal_coverage = alpha * 100,
                 lambda = "cv",
                 modifier = modifier)
arg_list2 <- list(data = data_type,
                 n = 100,
                 p = 100,
                 snr = 1,
                 sd = ifelse(data_type == "normal", sd, NA),
                 rate = ifelse(data_type == "laplace", rt, NA),
                 a = ifelse(data_type == "abn", 1, NA),
                 b = ifelse(data_type == "abn", 1, NA),
                 correlation_structure = "exchangeable",
                 correlation = 0.99,
                 correlation_noise = ifelse(data_type == "abn", 0, NA),
                 method = c("zerosample2"),
                 ci_method = "quantile",
                 nominal_coverage = alpha * 100,
                 lambda = "cv",
                 modifier = modifier)
methods_pretty <- c(methods_pretty, "ridge" = "Ridge")

cis <- list()
examples <- list()
params_grid <- rbind(expand.grid(arg_list), expand.grid(arg_list2))

for (i in 1:nrow(params_grid)) {
  read_objects(rds_path, params_grid[i,])
  cis[[i]] <- confidence_interval
  examples[[i]] <- example
}
names(cis) <- params_grid$method
names(examples) <- params_grid$method

plots <- list()
eplots <- list()
for (i in 1:nrow(params_grid)) {
  curr_method <- params_grid$method[i]
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

  pdat$variable <- factor(pdat$variable, levels = c("N1", "B1", "A1"))

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
    # annotate("text", x = 1.2, y = 3.4, label = methods_pretty[as.character(params_grid$method[i])], size = 5)
    ggtitle(methods_pretty[as.character(params_grid$method[i])])

 if (params_grid$method[i] == "zerosample2") {
   current_cis %>%
     filter(variable == "B1") %>%
     mutate(under0 = lower <= 0) %>%
     pull(under0) %>% mean() %>%
     print()
 }


 if (params_grid$method[i] == "ridge") {
   ridge_example <- current_example
   colnames(ridge_example) <- tolower(colnames(ridge_example))
   tmp_plot <- plot_ridge(ridge_example, n = 20)
 } else {
   tmp_plot <- plot(current_example, n = 20, original_order = TRUE)
 }
 plots[[2 + 2*(i-1)]] <- tmp_plot +
   coord_cartesian(xlim = c(-1, 2)) +
   ylab(NULL) +
   xlab(NULL)

}


left_label <- textGrob("Variable", gp = gpar(fontsize = 12), rot = 90)
bottom_label <- textGrob("Interval Endpoint", gp = gpar(fontsize = 12))

pdf("./fig/highcorr.pdf", height = length(params_grid$method) * 3)
(plots[[1]] + ylab("Variable") + plots[[2]]) / (plots[[3]] + xlab(expression(beta)) + ylab("Variable") + plots[[4]] + xlab(expression(beta)) + patchwork::plot_layout(axes = "collect"))
dev.off()

