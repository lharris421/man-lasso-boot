# Sparse 1: 1-10 = ±(0.5, 0.5, 0.5, 1, 2), 11−100 = 0; 0.09
# Sparse 2: 0.12
# Sparse 3: 0.20
# Laplace: 0.25
# T: 0.55
# Normal: 0.40
# Unif: 1.5

## Setup
source("./fig/setup.R")

simulation_info <- list(seed = 1234, iterations = 1000,
                        simulation_function = "gen_data_distribution", simulation_arguments = list(
                          p = 100, SNR = 1
                        ), script_name = "distributions")

## Load data back in
methods <- methods[c("lasso_boot")]
ns <- c(50, 100, 400)
# distributions <- c( "beta", "laplace", "normal", "t", "uniform", "beta",
#                     "sparse 1", "sparse 2", "sparse 3")
distributions <- c( "beta", "laplace", "normal", "t", "uniform", "beta")

files <- expand.grid("method" = names(methods), "n" = ns, "distribution" = distributions, stringsAsFactors = FALSE)

results <- list()
for (i in 1:nrow(files)) {

  simulation_info$simulation_arguments$n <- files[i,] %>% pull(n)
  simulation_info$simulation_arguments$distribution <- files[i,] %>% pull(distribution)

  results[[i]] <- indexr::read_objects(
    rds_path,
    c(methods[[files[i,"method"]]], simulation_info)
    # args
  ) %>%
    mutate(method = files[i,] %>% pull(method), distribution = files[i,] %>% pull(distribution), n = files[i,] %>% pull(n))
}

ps <- list(
  "laplace" = rlaplace(1000, rate = 1),
  "normal" = rnorm(1000),
  "t" = rt(1000, df = 3),
  "uniform" = runif(1000, -1, 1),
  "beta" = rbeta(1000, .1, .1) -.5,
  "sparse 1" = c(rep(c(rep(0.5, 30), rep(1, 10), rep(2, 10)), 2) * c(rep(1, 50), rep(-1, 50)), rep(0, 900)),
  "sparse 2" = c(rnorm(300), rep(0, 700)),
  "sparse 3" = c(rnorm(500), rep(0, 500))
)

results <- bind_rows(results) %>%
  rename(Distribution = distribution) %>%
  mutate(covered = lower <= truth & truth <= upper) %>%
  group_by(Distribution, n) %>%
  summarise(coverage = mean(covered)) %>%
  pivot_wider(names_from = n, values_from = coverage) %>%
  mutate(
    ` ` = "",
    Distribution = stringr::str_to_title(Distribution),
    Distribution = factor(Distribution, levels = stringr::str_to_title(names(ps))),
  ) %>%
  arrange(Distribution) %>%
  select(` `, Distribution, `50`, `100`, `400`)


ps <- ps[stringr::str_to_lower(results$Distribution)]

ps <- lapply(ps, function(x) x / max(abs(x)))
names(ps) <- paste0('distribution_table_', letters[1:length(ps)])

# Assuming wide_data is your data frame
kbl(results,
    format = "latex",
    align = "ccccc",  # Alignments for the columns
    booktabs = TRUE,
    digits = 3,
    linesep = "",
    table.envir = NULL) %>%
  add_header_above(c("  " = 2, "Sample Size" = 3)) %>%
  column_spec(1, image = spec_hist(ps, breaks = 20, dir='./fig', file_type='pdf')) %>%
  stringr::str_replace_all('file:.*?/fig/', '') %>%
  write('tab/distribution_table.tex')
