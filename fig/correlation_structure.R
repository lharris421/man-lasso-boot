## Setup
source("./fig/setup/setup.R")

dlaplace <- function(x, rate = 1) {
  dexp(abs(x), rate) / 2
}

plots <- list()

method <- "zerosample2"
data_types <- c("laplace", "abn", "abn")
corrs <- c("autoregressive", "exchangeable", "autoregressive")
rhos <- c(0.7, 0.5, 0.8)
addtl <- c(2, 5, 5)
p <- 100

SNR <- 1
per_var_data <- list()
alpha <- .2
ns <- c(30, 40, 80)

plots <- list()
for (i in 1:length(data_types)) {
  data_type <- data_types[i]
  rho <- rhos[i]
  corr <- corrs[i]
  ad_inf <- addtl[i]
  load(glue("{res_dir}/rds/{data_type}({ad_inf})_SNR{SNR}_{corr}_rho{rho*100}_{method}_alpha{alpha*100}_p{p}.rds"))
  per_var_data <- per_var
  print(glue("Data = {data_type}, Corr {corr} ({rho})"))
  per_var_data %>%
    mutate(covered = lower <= truth & upper >= truth) %>%
    group_by(n) %>%
    summarise(coverage = mean(covered)) %>%
    print()
  # plots[[i]] <- single_method_plot(per_var_data, ns, alpha) +
  #   # annotate("text", x = 1, y = 0.5, label = paste0("alpha = ", alpha), size = 5) +
  #   ggtitle(glue("Data = {data_type}, Corr {corr} ({rho})")) +
  #   theme(legend.position = "none")
}
#
# plots[[1]] <- plots[[1]] +
#   theme(legend.position = c(1, .05),
#         legend.justification = c("right", "bottom"),
#         legend.direction = "horizontal",
#         legend.box.just = "right",
#         legend.margin = margin(6, 6, 6, 6),
#         legend.background = element_rect(fill = "transparent"))
#
#
# # suppressMessages({
# pdf("./fig/correlation_structures.pdf", height = 3.5)
# g <- grid.arrange(grobs = plots, ncol = 3, nrow = 1)
# dev.off()
# })

