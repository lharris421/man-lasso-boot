## Setup
source("./fig/setup/setup.R")

## Load Data
data_type <- "whoari"
methods <- c("zerosample2", "blp", "selectiveinference")
alpha <- .2
lambda <- "cv"

params_grid <- expand.grid(list(data = data_type, method = methods, lambda = lambda,
                                ci_method = "quantile", nominal_coverage = alpha * 100))
# Fetching and combining data
cis <- list()
for (i in 1:nrow(params_grid)) {
  read_objects(rds_path, params_grid[i,])
  cis[[i]] <- res$confidence_interval
}
cis <- do.call(rbind, cis) %>% data.frame()

## Plotting
# suppressMessages({
  pdf("./fig/comparison_data.pdf", width = 7.5)
  plot_ci_comparison(cis) +
    coord_cartesian(xlim = c(-0.6, 0.6))
  dev.off()
  if (save_rds) {
    pobj <- plot_ci_comparison(cis) +
      coord_cartesian(xlim = c(-0.6, 0.6))
    save(pobj, file = glue("{res_dir}/web/rds/comparison_data.rds"))
  }
# })
