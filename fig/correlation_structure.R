## Setup
source("./fig/setup/setup.R")


methods <- "lasso"
n_values <- c(50, 100, 400)
data_type <- "laplace"
SNR <- 1
alpha <- .2
p <- 100
enet_alpha <- 1
corr <- c("autoregressive")
rhos <- rho <- c(0.4, 0.6, 0.8)

# Initialize an empty list to store data from all rho values
params_grid <- expand.grid(list(data = data_type, n = n_values, snr = SNR,
                                method = methods, lambda = "cv", alpha = enet_alpha,
                                nominal_coverage = (1-alpha) * 100, p = p,
                                correlation_structure = corr,
                                correlation = rho * 100))

# params_grid <- cbind(params_grid, modifier = rep(c(NA, NA, "debias"), each = 3))

per_var_data <- list()
for (j in 1:nrow(params_grid)) {
  res_list <- read_objects(rds_path, params_grid[j,])
  per_var_data[[j]] <- res_list$per_var_n %>%
    mutate(rho = params_grid[j, "correlation"])
}
combined_data <- do.call(rbind, per_var_data) %>%
  data.frame() %>%
  filter(submethod %in% c("hybrid"))


# Transform and summarize data
coverage_data <- combined_data %>%
  mutate(covered = lower <= truth & upper >= truth, n = as.factor(n), rho = factor(rho)) %>%
  group_by(group, submethod, rho, n) %>%
  summarise(coverage = mean(covered), .groups = 'drop')

# Create a single plot with facets for each rho
final_plot <- coverage_data %>%
  ggplot(aes(x = n, y = coverage, fill = n)) +
  geom_boxplot() +
  facet_wrap(submethod~rho, as.table = FALSE, labeller = label_bquote(.(methods_pretty[submethod]) - rho == .(rhos[rho]))) +
  labs(x = "Sample Size", y = "Coverage Rate") +
  theme_bw() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 1 - alpha) +
  coord_cartesian(ylim = c(0, 1))

# Print the plot

pdf("./fig/correlation_structure.pdf", width = 8, height = 4)
final_plot
dev.off()
