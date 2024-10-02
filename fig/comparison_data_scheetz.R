## Setup
source("./fig/setup/setup.R")

## Load Data
data_type <- "Scheetz2006"
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
  cis[[i]] <- res$confidence_interval

  if (params_grid$method[i] == "lasso") {
    cis[[i]] <- cis[[i]] %>%
      filter(method %in% c("hybrid"))
  }

}
cis <- do.call(dplyr::bind_rows, cis) %>% data.frame()



cis %>%
  mutate(lower_inf = is.infinite(lower), upper_inf = is.infinite(upper), both_inf = is.infinite(lower) & is.infinite(upper)) %>%
  group_by(method) %>%
  summarise(sum(lower_inf), sum(upper_inf), sum(both_inf), n())

cis %>%
  filter(variable %in% c("1375717_at", "1389759_at", "1378152_at"))

cis %>%
  mutate(sign_same = sign(lower) == sign(estimate) | sign(upper) == sign(estimate)) %>%
  group_by(method) %>%
  summarise(sum(sign_same))

cis %>%
  mutate(is_not_zero = lower > 0 | upper < 0) %>%
  filter(method != "selective_inference" & is_not_zero)
  # group_by(method) %>%
  # summarise(sum(is_not_zero))

pdf("./fig/comparison_data_scheetz.pdf", width = 8, height = 4)
plot_ci_comparison(cis, nvars = 30)
dev.off()
