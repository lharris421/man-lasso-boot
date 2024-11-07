source("./fig/setup.R")

alpha <- 0.2
for (i in 1:length(methods)) {
  methods[[i]]$method_arguments["alpha"] <- alpha
}

simulation_info <- list(seed = 1234, iterations = 1000,
                        simulation_function = "gen_data_distribution", simulation_arguments = list(
                          p = 100, SNR = 1, corr = "autoregressive", distribution = "laplace"
                        ), script_name = "distributions")

## Load data back in
methods <- methods[c("lasso_boot")]
ns <- c(50, 100, 400)
rhos <- c(0.4, 0.6, 0.8)

files <- expand.grid("n" = ns, "rho" = rhos, stringsAsFactors = FALSE)

results <- list()
results_per_sim <- list()
for (i in 1:nrow(files)) {

  simulation_info$simulation_arguments$n <- files[i,] %>% pull(n)
  simulation_info$simulation_arguments$rho <- files[i,] %>% pull(rho)

  results[[i]] <- indexr::read_objects(
    rds_path,
    c(methods[["lasso_boot"]], simulation_info)
    # args
  ) %>%
    mutate(
      n = factor(files[i,] %>% pull(n)),
      rho = factor(files[i,] %>% pull(rho))
    )

}

combined_data <- bind_rows(results)

# Transform and summarize data
coverage_data <- combined_data %>%
  mutate(covered = lower <= truth & upper >= truth, n = as.factor(n), rho = factor(rho)) %>%
  group_by(iteration, method, rho, n) %>%
  summarise(coverage = mean(covered), .groups = 'drop')

# Create a single plot with facets for each rho
final_plot <- coverage_data %>%
  ggplot(aes(x = n, y = coverage, fill = n)) +
  geom_boxplot() +
  facet_wrap(method~rho, as.table = FALSE, labeller = label_bquote(.(methods_pretty[method]) - rho == .(rhos[rho]))) +
  labs(x = "Sample Size", y = "Coverage Rate") +
  theme_bw() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 1 - alpha) +
  coord_cartesian(ylim = c(0, 1))

# Print the plot

pdf("./fig/correlation_structure.pdf", width = 8, height = 4)
final_plot
dev.off()
