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
  res <- read_objects(rds_path, params_grid[i,], save_method = "rds")
  cis[[i]] <- res$confidence_interval %>%
    select(estimate, lower, upper, variable, method)
  print(ncol(cis[[i]]))
  print(colnames(cis[[i]]))
}
cis <- do.call(rbind, cis) %>% data.frame()

## Plotting
pdf("./fig/comparison_data.pdf", width = 6, height = 4)
plot_ci_comparison(cis)
dev.off()
