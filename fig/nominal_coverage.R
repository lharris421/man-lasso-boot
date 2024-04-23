## Setup
source("./fig/setup/setup.R")

plots <- list()

method <- "zerosample2"
data_type <- "laplace"
corr <- "exchangeable"
rho <- 0

per_var_data <- list()
alphas <- c(0.05, 0.1, 0.2)
p <- 100
ns <- p * nprod
rate <- 2
SNR <- 1
modifier <- NA

library(dplyr)
library(ggplot2)
library(mgcv)  # For gam()

# Expand grid for all combinations
params_grid <- expand.grid(data = data_type, n = ns, snr = SNR, lambda = "cv",
                           method = method, ci_method = "quantile",
                           nominal_coverage = alphas * 100, p = p,
                           modifier = modifier, stringsAsFactors = FALSE)

# Function to read and process each combination, integrating single_method_plot logic
read_process_data <- function(params) {
  res_list <- read_objects(rds_path, params, save_method = "rds")
  print("1")
  print(length(res_list))
  per_var_data <- res_list$per_var_n
  cutoff <- round(quantile(abs(per_var_data$truth), .98), 1)

  tmp <- per_var_data %>%
    mutate(
      covered = lower <= truth & upper >= truth,
      mag_truth = abs(truth),
      covered = as.numeric(covered)
    ) %>%
    filter(n == params$n)
  print("2")
  print(nrow(tmp))

  fit <- gam(covered ~ s(mag_truth) + s(group, bs = "re"), data = tmp, family = binomial)
  xs <- seq(0, cutoff, by = .01)
  ys <- predict(fit, data.frame(mag_truth = xs, group = 101), type = "response")

  line_data <- data.frame(x = xs, y = ys, n = factor(params$n), alpha = params$nominal_coverage, which = "curve")
  mean_coverage <- data.frame(y = mean(tmp$covered), x = xs, n = factor(params$n), alpha = params$nominal_coverage, which = "mean")
  line_data <- bind_rows(line_data, mean_coverage)

  return(line_data)
}

# Apply the function across all parameter combinations and bind rows
all_data <- do.call(rbind, lapply(1:nrow(params_grid), function(i) read_process_data(params_grid[i, ])))


# Plotting with facet wrap
ggplot() +
  geom_line(data = all_data %>% filter(which == "curve"), aes(x = x, y = y, color = n)) +
  geom_line(data = all_data %>% filter(which == "mean"), aes(x = x, y = y, color = n), lty = "dashed") +
  facet_wrap(.~alpha, labeller = label_bquote(alpha == .(alpha / 100))) +
  geom_hline(data = all_data, aes(yintercept = (1 - as.numeric(alpha)/100)), color = "black") +
  theme_bw() +
  labs(x = expression(abs(beta)), y = "Estimated Coverage Probability") +
  ggtitle("Coverage Probability by Alpha and Sample Size") +
  scale_color_manual(name = "N", values = colors)
