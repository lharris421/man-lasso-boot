## Setup
source("./fig/setup/setup.R")

dlaplace <- function(x, rate = 1) {
  dexp(abs(x), rate) / 2
}

plots <- list()

# methods <- c("zerosample2")
# n_values <- c(50, 100, 400) # ns values you are interested in
# data_types <- c("laplace", "abn", "abn")
# corrs <- c("autoregressive", "exchangeable", "autoregressive")
# rhos <- c(0.7, 0.5, 0.8)
# rhos.noise <- c()
# rate <- c(2, NA, NA)
# a <- c(NA, 5, 5)
# b <- c(NA, 2, 2)
# SNR <- 1
# alpha <- .2
# p <- 100
#
# params_grid <- data.frame(
#   method = methods,
#   data_type = data_types,
#   correlation_structure = corrs,
#   correlation = rhos
# )

methods <- c("zerosample2")
n_values <- c(50, 100, 400) # ns values you are interested in
data_type <- c("laplace", "laplace", "laplace", "abn", "abn", "abn")
rate <- c(2, 2, 2, NA, NA, NA)
a <- c(NA, NA, NA, 5, 5, 5)
b <- c(NA, NA, NA, 2, 2, 2)
SNR <- 1
corr <- c("autoregressive", "autoregressive", "autoregressive", "exchangeable", "exchangeable", "exchangeable")
rho <- c(.4, .7, .95, .5, .8, .99)
rho_noise <- c(NA, NA, NA, .2, .5, .6)
alpha <- .2
p <- 100
modifier <- NA


# methods <- c("zerosample2")
# n_values <- c(50, 100, 400, 1000) # ns values you are interested in
# data_type <- c("laplace")
# rate <- c(2)
# a <- c(NA)
# b <- c(NA)
# SNR <- 1
# corr <- c("autoregressive")
# rho <- c(.95)
# rho_noise <- c(NA)
# alpha <- .2
# p <- 100
# modifier <- NA

plots <- list()
for (i in 1:length(rho)) {

  params_grid <- expand.grid(list(data = data_type[i], n = n_values,
                                  rate = rate[i], a = a[i], b = b[i],
                                  snr = SNR,
                                  correlation_structure = corr[i], correlation = rho[i] * 100, correlation_noise = rho_noise[i] * 100,
                                  method = methods,
                                  ci_method = "quantile", nominal_coverage = alpha * 100, p = p, modifier = modifier))
  per_var_data <- list()
  for (j in 1:nrow(params_grid)) {
    read_objects(rds_path, params_grid[j,])
    per_var_data[[j]] <- per_var_n %>%
      filter(n == params_grid[j, "n"])
  }
  per_var_data <- do.call(rbind, per_var_data) %>%
    data.frame()

  per_var_data %>%
    mutate(covered = lower <= truth & upper >= truth, n = as.factor(n)) %>%
    group_by(n) %>%
    summarise(coverage = mean(covered)) %>%
    print()

  plots[[i]] <- per_var_data %>%
    mutate(covered = lower <= truth & upper >= truth, n = as.factor(n)) %>%
    group_by(group, n) %>%
    summarise(coverage = mean(covered)) %>%
    ggplot(aes(x = n, y = coverage, fill = n)) +
    geom_boxplot() +
    ggtitle(glue("{corr[i]} ({rho[i]})")) +
    theme_bw() +
    theme(legend.position = "none") +
    coord_cartesian(ylim = c(ifelse(i > 3, .8, .5), 1)) +
    geom_hline(yintercept = 1 - alpha)

}


plots[[1]] <- plots[[1]] +
  theme(legend.position = c(1.02, 0),
        legend.justification = c("right", "bottom"),
        legend.direction = "horizontal",
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.background = element_rect(fill = "transparent"))
#
#
# suppressMessages({
pdf("./fig/correlation_structure.pdf", width = 7.5, height = 4)
g <- grid.arrange(grobs = plots, ncol = 3, nrow = 2)
dev.off()
# })

