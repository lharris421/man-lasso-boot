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
alphas <- c(0.05, 0.1, 0.2)
p <- 100
ns <- p * c(0.75, 1, 4)
rate <- 2
SNR <- 1

plots <- list()
for (i in 1:length(alphas)) {
  alpha <- alphas[i]
  load(glue("{res_dir}/rds/{data_type}({rate})_SNR{SNR}_{corr}_rho{rho*100}_{method}_alpha{alpha*100}_p{p}.rds"))
  per_var_data <- per_var
  plots[[i]] <- single_method_plot(per_var_data, ns, alpha) +
    # annotate("text", x = 1, y = 0.5, label = paste0("alpha = ", alpha), size = 5) +
    ggtitle(paste0("alpha = ", alpha)) +
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
pdf("./fig/nominal_coverage.pdf", height = 3.5)
g <- grid.arrange(grobs = plots, ncol = 3, nrow = 1)
dev.off()
# })

