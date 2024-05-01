## Data arguments
source("./fig/setup/setup.R")

data_type <- "laplace-single"
p <- 100
ns <- 100
nboot <- 1000
alpha <- .2
SNR <- 1

methods <- c("debiased")
n_methods <- length(methods)
ci_method <- "quantile"

args_list <- list(data = data_type,
                  n = ns,
                  snr = SNR,
                  method = methods,
                  ci_method = ci_method,
                  nominal_coverage = alpha * 100,
                  lambda = "cv",
                  p = p)

res_list <- read_objects(rds_path, expand.grid(args_list), save_method = "rds")

res <- res_list$res
truth_df <- res_list$truth_df

which_cols <- 1:100
# which_cols <- which_cols[order(truth_df$truth[which_cols])]

plot_list <- list()

# Create a combined dataset with a column to identify each variable
data <- data.frame(value = as.vector(res$draws[, which_cols]),
                   variable = rep(which_cols, each = nrow(res$draws)))

# Calculate the quantiles and estimates for each variable using reframe()
lines_data <- data %>%
  group_by(variable) %>%
  reframe(
    ci_low = quantile(value, 0.1),
    ci_high = quantile(value, 0.9),
    est = res$estimates[variable],
    true_val = truth_df$truth[variable]
  ) %>%
  ungroup() %>%
  filter((ci_low > true_val & sign(ci_low) == 1 & sign(true_val) == 1) |
           (ci_high < true_val & sign(ci_low) == -1 & sign(true_val) == -1))# Ensure the data frame is ungrouped

which_cols <- unique(lines_data$variable)

# Determine the global x-axis limits
min_x <- min(res$draws[, which_cols])
max_x <- max(res$draws[, which_cols])

data <- data %>% filter(variable %in% which_cols)

# Sort and convert 'variable' to a factor explicitly
lines_data <- lines_data %>%
  distinct(variable, .keep_all = TRUE) %>%
  arrange(true_val) %>%
  mutate(variable = factor(variable, levels = order(truth_df$truth)))  # ensure variable is a factor

# Apply the same factor levels to the main data
data$variable <- factor(data$variable, levels = levels(lines_data$variable))
cvariable <- character(100)
cvariable[unique(data$variable)] <- as.character(unique(data$variable))


# Plotting
p <- ggplot(data, aes(x = value)) +
  geom_histogram(bins = 20, fill = "grey", color = "black", alpha = 0.5) +
  geom_vline(data = lines_data, aes(xintercept = ci_low, color = "Confidence Interval"), linetype = "dashed", linewidth = 1) +
  geom_vline(data = lines_data, aes(xintercept = ci_high, color = "Confidence Interval"), linetype = "dashed", linewidth = 1) +
  geom_vline(data = lines_data, aes(xintercept = est, color = "Estimate"), linewidth = 1) +
  geom_vline(data = lines_data, aes(xintercept = true_val, color = "True Value"), linewidth = 1) +
  scale_color_manual(values = c("Confidence Interval" = "red", "Estimate" = "blue", "True Value" = "black")) +
  facet_wrap(~ variable, scales = "fixed", ncol = 3, dir = "v", labeller = label_bquote(beta[.(cvariable[variable])])) +
  labs(x = expression(beta), y = "Frequency",
       color = "") +  # Legend title
  theme_minimal() +
  theme(legend.position = "top") +  # Position the legend at the top
  coord_cartesian(xlim = c(min_x, max_x))


pdf("./fig/debiased_example.pdf", height = 4, width = 8)
p
dev.off()
