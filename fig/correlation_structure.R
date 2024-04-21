## Setup
source("./fig/setup/setup.R")

methods <- c("zerosample2")
             # "debiased")
ns <- c(50, 100, 400) # ns values you are interested in
data_type <- c("laplace")
rate <- c(2)
SNR <- 1
corr <- c("autoregressive")
rhos <- rho <- c(.4, .6, .8)
alpha <- .2
p <- 100

# Initialize an empty list to store data from all rho values
args_list <- list(data = data_type,
                  n = ns,
                  snr = SNR,
                  correlation_structure = corr,
                  correlation = rho * 100,
                  method = methods,
                  ci_method = ci_method,
                  nominal_coverage = alpha * 100,
                  lambda = "cv",
                  p = p)
params_grid <- expand.grid(args_list)


per_var_data <- list()
for (j in 1:nrow(params_grid)) {
  res_list <- read_objects(rds_path, params_grid[j,], save_method = "rds")
  per_var_data[[j]] <- res_list$per_var_n %>%
    mutate(rho = params_grid[j, "correlation"])
}
combined_data <- do.call(rbind, per_var_data) %>%
  data.frame()


# Transform and summarize data
coverage_data <- combined_data %>%
  mutate(covered = lower <= truth & upper >= truth, n = as.factor(n), rho = factor(rho)) %>%
  group_by(group, method, rho, n) %>%
  summarise(coverage = mean(covered), .groups = 'drop')

# Create a single plot with facets for each rho
final_plot <- coverage_data %>%
  ggplot(aes(x = n, y = coverage, fill = n)) +
  geom_boxplot() +
  facet_wrap(method~rho, scales = "free_x", labeller = label_bquote(rho == .(rhos[rho]))) +
  labs(x = "Sample Size", y = "Coverage Probability", title = "Coverage Probability by Sample Size and Correlation") +
  theme_bw() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 1 - alpha)

# Print the plot

pdf("./fig/correlation_structure.pdf", width = 6, height = 4)
final_plot
dev.off()
