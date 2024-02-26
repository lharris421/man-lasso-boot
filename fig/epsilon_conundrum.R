## Setup
source("./fig/setup/setup.R")

## Load Data
methods <- c("traditional", "zerosample2")
n_values <- 100
data_type <- "sparse"
SNR <- 1
corr <- "exchangeable"
rho <- 0
alpha <- .2
p <- 100
modifier <- NA

params_grid <- expand.grid(list(data = data_type, n = n_values, snr = SNR, lambda = "cv",
                                correlation_structure = corr, correlation = rho, method = methods,
                                ci_method = "quantile", nominal_coverage = alpha * 100, p = p, modifier = modifier))

# Fetching and combining data
res_ci <- list()
res <- list()
coverages <- numeric()
for (i in 1:nrow(params_grid)) {
  read_objects(rds_path, params_grid[i,])
  res_ci[[i]] <- confidence_interval
  res[[i]] <- example
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

## Traditional Bootstrap
plots <- list()
for (i in 1:length(methods)) {
  ci <- ci.boot.ncvreg(res[[i]], ci_method = ci_method, original_data = dat)
  # cov <- mean(ci$lower <= dat$beta & dat$beta <= ci$upper) ## Traditional
  cov <- coverages[methods[i]]
  plots[[i]] <- plot(res[[i]], n = 30, ci_method = ci_method, original_data = dat) +
    ggtitle(glue("{methods_pretty[methods[i]]} - Coverage: {round(cov * 100, 1)} %")) +
    ylab(NULL) +
    theme(
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank()
    )
}


left_label <- textGrob("Variable", gp = gpar(fontsize = 10), rot = 90)

suppressMessages({
  pdf("./fig/epsilon_conundrum.pdf", height = 4 * ceiling(length(methods) / 2))
  grid.arrange(grobs = plots, nrow = ceiling(length(methods) / 2), left = left_label)
  dev.off()
  if (save_rds) {
    gobj <- grid.arrange(grobs = plots, nrow = ceiling(length(methods) / 2), left = left_label)
    save(gobj, file = glue("{res_dir}/web/rds/epsilon_conundrum.rds"))
  }
})
