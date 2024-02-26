## Setup
source("./fig/setup/setup.R")

dlaplace <- function(x, rate = 1) {
  dexp(abs(x), rate) / 2
}

plots <- list()

method <- "zerosample2"
data_type <- "laplace"
corr <- "exchangeable"
rho <- 0

per_var_data <- list()
alpha <- .2
p <- 100
ns <- p * nprod
rate <- 2
SNR <- 1
modifier <- c("tl", "tls")

new_folder <- "/Users/loganharris/github/lasso-boot/new_rds/"

# Fetching and combining data
plots <- list()
for (i in 1:length(modifier)) {
  per_var_data <- list()
  params_grid <- expand.grid(list(data = data_type, n = ns, rate = rate, snr = SNR,
                                  correlation_structure = corr, correlation = rho, method = method,
                                  ci_method = "quantile", nominal_coverage = alpha * 100, p = p, modifier = modifier[i]))
  for (j in 1:length(ns)) {
    read_objects(rds_path, params_grid[j,])
    per_var_data[[j]] <- per_var_n
  }
  per_var_data <- do.call(rbind, per_var_data) %>%
    data.frame()
  plots[[i]] <- single_method_plot(per_var_data, ns, alpha) +
    ggtitle(paste0("Modifier = ", params_grid$modifier)) +
    theme(legend.position = "none")
}

plots[[1]] <- plots[[1]] +
  theme(legend.position = c(1, .05),
        legend.justification = c("right", "bottom"),
        legend.direction = "horizontal",
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.background = element_rect(fill = "transparent"))


# suppressMessages({
pdf("./fig/true_lambda.pdf", height = 3.5)
g <- grid.arrange(grobs = plots, ncol = 2, nrow = 1)
dev.off()
# })

