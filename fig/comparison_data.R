## Setup
source("./fig/setup/setup.R")

## Load Data
data_type <- "whoari"
methods <- c("lasso", "blpmin", "selectiveinference")
alpha <- .2
lambda <- "cv"

params_grid <- expand.grid(list(data = data_type, method = methods, lambda = lambda,
                                nominal_coverage = (1-alpha) * 100))
params_grid <- cbind(params_grid, alpha = c(1, NA, NA))

# Fetching and combining data
cis <- list()
for (i in 1:nrow(params_grid)) {
  res <- read_objects(rds_path, params_grid[i,])
  cis[[i]] <- res$confidence_interval %>%
    select(estimate, lower, upper, variable, method)

  if (params_grid$method[i] == "lasso") {
    cis[[i]] <- cis[[i]] %>%
      filter(method %in% c("hybrid"))
  }

  print(ncol(cis[[i]]))
  print(colnames(cis[[i]]))
}
cis <- do.call(rbind, cis) %>% data.frame()

cis %>%
  mutate(is_not_zero = lower > 0 | upper < 0) %>%
  group_by(method) %>%
  summarise(sum(is_not_zero))


## Plotting
pdf("./fig/comparison_data.pdf", width = 8, height = 5)
plot_ci_comparison(cis, nvars = 66)
dev.off()
