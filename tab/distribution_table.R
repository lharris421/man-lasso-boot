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

methods <- c("zerosample2")
n_values <- c(50, 100, 400) # ns values you are interested in
data_type <- c("laplace", "normal", "t", "uniform", "beta", "sparse 1", "sparse 2", "sparse 3")
SNR <- 1
alpha <- .2
p <- 100

params_grid <- expand.grid(list(data = data_type, n = n_values, snr = SNR,
                                method = methods, lambda = "cv",
                                ci_method = "quantile", nominal_coverage = alpha * 100, p = p))


# Fetching and combining data
per_var_data <- list()
for (i in 1:nrow(params_grid)) {
  res_list <- read_objects(rds_path, params_grid[i,], save_method = "rds")
  per_var_n <- res_list$per_var_n
  per_var_data[[i]] <- per_var_n %>%
    mutate(data_type = params_grid[i,"data"])
}
per_var_data <- do.call(rbind, per_var_data) %>%
  data.frame() %>%
  mutate(
    covered = lower <= truth & upper >= truth,
    method = methods_pretty[method]
  ) %>%
  group_by(data_type, n) %>%
  summarise(Coverage = mean(covered) * 100) %>%
  ungroup()


wide_data <- per_var_data %>%
  pivot_wider(names_from = n, values_from = Coverage) %>%
  rename(Distribution = data_type) %>%
  mutate(Distribution = stringr::str_to_title(Distribution))

# Convert the reshaped data frame to a LaTeX table
latex_table <- xtable(wide_data,
                      align = "cl|ccc",  # 'l' for left-aligned distribution name, '|' then 'ccc' for centered columns
                      caption = "Coverage by Data Type and Sample Size",
                      digits = 1,
                      label = "tab:coverage")

# Customizing the print output to match your desired LaTeX format
print(latex_table,
      include.rownames = FALSE,
      sanitize.text.function = function(x){x},  # Avoid converting underscores and other special characters
      hline.after = c(0, nrow(wide_data)),  # Horizontal lines after the header and the last row
      add.to.row = list(pos = list(-1),
                        command = c("\\hline & \\multicolumn{3}{c}{Sample Size} \\\\\n")),  # Multi-column header
      comment = FALSE,
      file = "./tab/distribution_table.txt")  # Output to file


