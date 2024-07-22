## Setup
source("./fig/setup/setup.R")

## Load Data
data_type <- "whoari"
methods <- c("blp")
alpha <- .2
lambda <- "cv"

params_grid <- expand.grid(list(data = data_type, method = methods, lambda = lambda,
                                nominal_coverage = (1-alpha) * 100))

indexr:::generate_hash(list(data = data_type, method = methods, lambda = lambda,
                            nominal_coverage = (1-alpha) * 100))
# Fetching and combining data
cis <- list()
for (i in 1:nrow(params_grid)) {
  res <- read_objects(rds_path, params_grid[i,], save_method = "rds")
  cis[[i]] <- res$confidence_interval
}
cis <- do.call(dplyr::bind_rows, cis) %>% data.frame()

## Plotting
pdf("./fig/comparison_data_whoari_original.pdf", width = 8, height = 5)
plot_ci_comparison(cis, nvars = 66)
dev.off()
