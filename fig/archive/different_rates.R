## Setup
source("./fig/setup/setup.R")

dlaplace <- function(x, rate = 1) {
  dexp(abs(x), rate) / 2
}

plots <- list()

methods <- c("zerosample2")
n_values <- c(75, 100, 400) # ns values you are interested in
data_type <- "laplace"
rate <- 2
SNR <- 1
alpha <- .2
p <- 100
modifier <- NA
data_type <- "laplace"
corr <- "exchangeable"
rho <- 0

per_var_data <- list()
alpha <- .2
rates <- c(2, 10)

plots <- list()
for (i in 1:length(rates)) {
  rate <- rates[i]
  load(glue("{res_dir}/rds/{data_type}({rate})_SNR{1}_{corr}_rho{rho*100}_{method}_alpha{alpha*100}_p{p}.rds"))
  per_var_data <- per_var
  ns <- sort(unique(per_var$n))
  plots[[i]] <- single_method_plot(per_var_data, ns, alpha) +
    # annotate("text", x = 1, y = 0.5, label = paste0("alpha = ", alpha), size = 5) +
    ggtitle(paste0("Rate = ", rate)) +
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
pdf("./fig/different_rates.pdf", height = 3.5)
g <- grid.arrange(grobs = plots, ncol = length(rates), nrow = 1)
dev.off()
# })

