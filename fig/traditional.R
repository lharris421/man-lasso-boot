## Setup
source("./fig/setup/setup.R")

## Load Data
methods <- c("traditional", "zerosample2")
n_values <- 100
data_type <- "sparse"
SNR <- 1
alpha <- .2
p <- 100
ci_method <- "quantile"

params_grid <- expand.grid(list(data = data_type, n = n_values, snr = SNR, lambda = "cv",
                                method = methods,
                                ci_method = ci_method, nominal_coverage = alpha * 100, p = p))

# Fetching and combining data
res_ci <- list()
res <- list()
coverages <- numeric()
for (i in 1:nrow(params_grid)) {
  res_list <- read_objects(rds_path, params_grid[i,], save_method = "rds")
  res_ci[[i]] <- res_list$confidence_interval
  res[[i]] <- res_list$example
}
names(res_ci) <- methods
names(res) <- methods
for (i in 1:length(methods)) {
  coverages[i] <- do.call(rbind, res_ci[[methods[i]]]) %>%
    data.frame() %>%
    mutate(covered = lower <= truth & upper >= truth) %>%
    pull(covered) %>%
    mean()
}
names(coverages) <- methods
true_vals <- res_ci[[methods[i]]][[100]]$truth
kept_vars <- names(sort(abs(res[[1]]$estimates), decreasing = TRUE))[1:30]
true_vals <- true_vals[kept_vars]

## Traditional Bootstrap
plots <- list()
for (i in 1:length(methods)) {
  ci <- ci.boot.ncvreg(res[[i]])
  cov <- coverages[methods[i]]
  plots[[i]] <- plot(res[[i]], n = 30) +
    ggtitle(glue("{methods_pretty[methods[i]]} - Coverage: {round(cov * 100, 1)} %")) +
    ylab("Variable") +
    geom_point(data = data.frame(y = names(true_vals), x = true_vals), aes(x = x, y = y), color = "red", shape = "|") +
    theme(
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank()
    )
}


left_label <- textGrob("Variable", gp = gpar(fontsize = 10), rot = 90)

pdf("./fig/zerosample2.pdf", height = 4, width = 7)
plots[[2]]
dev.off()
pdf("./fig/traditional.pdf", height = 4, width = 7)
plots[[1]]
dev.off()

