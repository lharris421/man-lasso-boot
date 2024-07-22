## Setup
source("./fig/setup/setup.R")

## Load Data
data_type <- "abn"
alpha <- .2
arg_list <- list(data = data_type,
                 n = 100,
                 p = 100,
                 snr = 1,
                 a = ifelse(data_type == "abn", 1, NA),
                 b = ifelse(data_type == "abn", 1, NA),
                 correlation_structure = "exchangeable",
                 correlation = 0.99,
                 correlation_noise = ifelse(data_type == "abn", 0, NA),
                 method = c("ridge"),
                 nominal_coverage = (1-alpha) * 100,
                 lambda = "cv",
                 alpha = NA,
                 gamma = NA,
                 modifier = NA)
arg_list2 <- list(data = data_type,
                  n = 100,
                  p = 100,
                  snr = 1,
                  a = ifelse(data_type == "abn", 1, NA),
                  b = ifelse(data_type == "abn", 1, NA),
                  correlation_structure = "exchangeable",
                  correlation = 0.99,
                  correlation_noise = ifelse(data_type == "abn", 0, NA),
                  method = "lasso",
                  nominal_coverage = (1-alpha) * 100,
                  alpha = 0.8,
                  gamma = NA,
                  lambda = "cv",
                  modifier = NA)
arg_list3 <- list(data = data_type,
                  n = 100,
                  p = 100,
                  snr = 1,
                  a = ifelse(data_type == "abn", 1, NA),
                  b = ifelse(data_type == "abn", 1, NA),
                  correlation_structure = "exchangeable",
                  correlation = 0.99,
                  correlation_noise = ifelse(data_type == "abn", 0, NA),
                  method = "lasso",
                  nominal_coverage = (1-alpha) * 100,
                  alpha = 1,
                  gamma = NA,
                  lambda = "cv",
                  modifier = NA)

cis <- list()
examples <- list()
params_list <- list(
  expand.grid(arg_list), expand.grid(arg_list2), expand.grid(arg_list3)
)

for (i in 1:length(params_list)) {
  res_list <- read_objects(rds_path, params_list[[i]], save_method = "rds")
  cis[[i]] <- res_list$confidence_interval
  examples[[i]] <- res_list$example
}

methods <- c("ridge", "enet1", "lasso")
names(cis) <- methods
names(examples) <- methods

plots <- list()
eplots <- list()
for (i in 1:length(params_list)) {
  curr_method <- methods[i]
  if (curr_method == "ridge") {
    current_cis <- do.call(rbind, cis[[curr_method]]) %>%
      data.frame()
  } else {
    current_cis <- do.call(rbind, cis[[curr_method]]) %>%
      data.frame() %>%
      filter(submethod == "hybrid") %>%
      # filter(submethod == "posterior") %>%
      mutate(method = curr_method)
  }
  current_example <- examples[[curr_method]]

  if (curr_method == "ridge") {
    variables <- as.vector(sapply(cis[[curr_method]], function(x) rownames(x)))
    pdat <- current_cis %>%
      mutate(variable = variables) %>%
      filter(variable %in% c("A1", "B1", "N1")) %>%
      pivot_longer(lower:upper, names_to = "bound", values_to = "value") %>%
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
    ) +
    ylab(NULL) +
    xlab(NULL) +
    scale_color_manual(values = colors) +
    ggtitle(methods_pretty[as.character(curr_method)])

 if (curr_method == "ridge") {
   ridge_example <- current_example
   colnames(ridge_example) <- tolower(colnames(ridge_example))
   tmp_plot <- plot_ridge(ridge_example, n = 20)
 } else {
   tmp_plot <- plot(current_example, n = 20, method = "hybrid")
   # tmp_plot <- plot(current_example, n = 20, method = "posterior")
 }
 plots[[2 + 2*(i-1)]] <- tmp_plot +
   coord_cartesian(xlim = c(-1, 2)) +
   ylab(NULL) +
   xlab(NULL)

}


left_label <- textGrob("Variable", gp = gpar(fontsize = 12), rot = 90)
bottom_label <- textGrob("Interval Endpoint", gp = gpar(fontsize = 12))

pdf("./fig/highcorr.pdf", height = length(params_list) * 3)
(plots[[1]] + ylab("Variable") + plots[[2]]) /
  (plots[[3]] + ylab("Variable") + plots[[4]]) /
  (plots[[5]] + xlab(expression(beta)) + ylab("Variable") + plots[[6]] + xlab(expression(beta)) + patchwork::plot_layout(axes = "collect"))
dev.off()

