# Sparse 1: 1-10 = ±(0.5, 0.5, 0.5, 1, 2), 11−100 = 0; 0.09
# Sparse 2: 0.12
# Sparse 3: 0.20
# Laplace: 0.25
# T: 0.55
# Normal: 0.40
# Unif: 1.5

## Setup
source("./fig/setup/setup.R")

plots <- list()

methods <- c("lasso")
n_values <- c(50, 100, 400, 1000) # ns values you are interested in
data_type <- c("laplace", "normal", "t", "uniform", "beta", "sparse 1", "sparse 2", "sparse 3")
SNR <- 1
alpha <- .2
p <- 100

params_grid <- expand.grid(list(data = data_type, n = n_values, snr = SNR,
                                method = methods, lambda = "cv",
                                nominal_coverage = (1-alpha) * 100,
                                p = p, alpha = 1))


# Fetching and combining data
per_var_data <- list()
for (i in 1:nrow(params_grid)) {
  res_list <- read_objects(rds_path, params_grid[i,], save_method = "rds")
  per_var_n <- res_list$per_var_n
  per_var_data[[i]] <- per_var_n %>%
    mutate(data_type = params_grid[i,"data"])
}
per_var_data_hybrid <- do.call(rbind, per_var_data) %>%
  data.frame() %>%
  filter(submethod %in% c("hybrid")) %>%
  mutate(
    covered = lower <= truth & upper >= truth,
    method = methods_pretty[submethod]
  ) %>%
  group_by(data_type, n) %>%
  summarise(Coverage = mean(covered) * 100) %>%
  ungroup()

wide_data <- per_var_data_hybrid %>%
  pivot_wider(names_from = n, values_from = Coverage) %>%
  rename(Distribution = data_type) %>%
  mutate(Distribution = stringr::str_to_title(Distribution),
         ` ` = "") %>%
  select(` `, Distribution, `50`, `100`, `400`, `1000`)

ps <- list(
  rlaplace(1000, rate = 1), rnorm(1000), rt(1000, df = 3), runif(1000, -1, 1),
  rbeta(1000, .1, .1) -.5,
  c(rep(c(rep(0.5, 30), rep(1, 10), rep(2, 10)), 2) * c(rep(1, 50), rep(-1, 50)), rep(0, 900)),
  c(rnorm(300), rep(0, 700)),
  c(rnorm(500), rep(0, 500))
)

ps <- lapply(ps, function(x) x / max(abs(x)))
names(ps) <- paste0('distribution_table_', letters[1:length(ps)])

# Assuming wide_data is your data frame
kbl(wide_data,
    format = "latex",
    align = "cccccc",  # Alignments for the columns
    booktabs = TRUE,
    digits = 1,
    linesep = "",
    table.envir = NULL) %>%
  add_header_above(c("  " = 2, "Sample Size" = 4)) %>%
  column_spec(1, image = spec_hist(ps, breaks = 20, dir='./fig', file_type='pdf')) %>%
  stringr::str_replace_all('file:.*?/fig/', '') %>%
  write('tab/distribution_table.tex')
