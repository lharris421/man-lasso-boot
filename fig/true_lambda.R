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
ns <- c(30, 40, 80)
alts <- c("l", "ls")

plots <- list()
for (i in 1:length(alts)) {
  alt <- alts[i]
  load(glue("{res_dir}/rds/{data_type}_{corr}_rho{rho*100}_{method}_alpha{alpha*100}_true{alt}.rds"))
  per_var_data <- per_var
  plots[[i]] <- single_method_plot(per_var_data, ns, alpha) +
    # annotate("text", x = 1, y = 0.5, label = paste0("alpha = ", alpha), size = 5) +
    ggtitle(paste0("Alt = ", alt)) +
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

