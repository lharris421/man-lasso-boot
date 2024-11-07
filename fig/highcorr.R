source("./fig/setup.R")

alpha <- 0.2
for (i in 1:length(methods)) {
  methods[[i]]$method_arguments["alpha"] <- alpha
}

simulation_info <- list(seed = 1234, iterations = 1000,
                        simulation_function = "gen_data_distribution", simulation_arguments = list(
                        ), script_name = "distributions")

## Load data back in
methods <- methods[c("lasso_boot", "elastic_net", "ridge")]
ns <- c(100)
distributions <- c( "high_corr")

files <- expand.grid("method" = names(methods), "n" = ns, "distribution" = distributions, stringsAsFactors = FALSE)

results <- list()
for (i in 1:nrow(files)) {

  simulation_info$simulation_arguments$n <- files[i,] %>% pull(n)
  simulation_info$simulation_arguments$distribution <- files[i,] %>% pull(distribution)

  results[[i]] <- indexr::read_objects(
    rds_path,
    c(methods[[files[i,"method"]]], simulation_info)
    # args
  ) %>%
    mutate(method = files[i,] %>% pull(method), distribution = files[i,] %>% pull(distribution), n = files[i,] %>% pull(n))
}

results <- bind_rows(results)

plots <- list()
methods <- names(methods)
selected_example <- sample(1:1000, 1)
for (i in 1:length(methods)) {
  curr_method <- methods[i]

  tmp_results <- results %>%
    filter(method == curr_method)

  example_res <- tmp_results %>%
    filter(iteration == selected_example) %>%
    filter(variable %in% c("A1", "B1", glue::glue("N{1:18}")))

  pdat <- tmp_results %>%
    filter(variable %in% c("A1", "B1", "N1")) %>%
    pivot_longer(lower:upper, names_to = "bound", values_to = "value")

  pdat$variable <- factor(pdat$variable, levels = c("N1", "B1", "A1"))

 plots[[1 + 2*(i-1)]] <- pdat %>%
    ggplot() +
    geom_boxplot(aes(x = value, y = variable, color = bound), outliers = FALSE) +
    theme_bw() +
    coord_cartesian(xlim = c(-1, 2)) +
    theme(
      legend.position = "none"
    ) +
    ylab(NULL) +
    xlab(NULL) +
    scale_color_manual(values = colors) +
    ggtitle(methods_pretty[as.character(curr_method)])


 plots[[2 + 2*(i-1)]] <- example_res %>%
   ggplot() +
   geom_errorbar(aes(xmin = lower, xmax = upper, y = variable)) +
   geom_point(aes(x = estimate, y = variable)) +
   theme_bw() +
   coord_cartesian(xlim = c(-1, 2)) +
   ylab(NULL) +
   xlab(NULL)

}


left_label <- textGrob("Variable", gp = gpar(fontsize = 12), rot = 90)
bottom_label <- textGrob("Interval Endpoint", gp = gpar(fontsize = 12))

pdf("./fig/highcorr.pdf", height = nrow(files) * 3)
(plots[[1]] + ylab("Variable") + plots[[2]]) /
  (plots[[3]] + ylab("Variable") + plots[[4]]) /
  (plots[[5]] + xlab(expression(beta)) + ylab("Variable") + plots[[6]] + xlab(expression(beta)) + patchwork::plot_layout(axes = "collect"))
dev.off()

