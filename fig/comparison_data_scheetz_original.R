## Setup
source("./fig/setup/setup.R")

## Load Data
data_type <- "Scheetz2006"
methods <- c("blp")
alpha <- .2
lambda <- "cv"

params_grid <- expand.grid(list(data = data_type, method = methods, lambda = lambda,
                                nominal_coverage = (1-alpha) * 100))

# Fetching and combining data
cis <- list()
for (i in 1:nrow(params_grid)) {
  res <- read_objects(rds_path, params_grid[i,], save_method = "rds")
  cis[[i]] <- res$confidence_interval
}
cis <- do.call(dplyr::bind_rows, cis) %>% data.frame()

## Plotting
pdf("./fig/comparison_data_scheetz_original.pdf", width = 7, height = 3)
plot_ci_comparison(cis, nvars = 20)
dev.off()
