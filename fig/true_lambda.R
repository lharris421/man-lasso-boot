## Setup
source("./fig/setup/setup.R")

plots <- list()

method <- "lasso"
data_type <- "laplace"

per_var_data <- list()
alphas <- c(0.2)
p <- 100
ns <- p * nprod
rate <- 2
SNR <- 1
modifier <- c("tl", "tls")

library(dplyr)
library(ggplot2)
library(mgcv)  # For gam()

# Expand grid for all combinations
params_grid <- expand.grid(data = data_type, n = ns, snr = SNR, lambda = "cv",
                           method = method,
                           nominal_coverage = (1-alphas) * 100, p = p,
                           modifier = modifier, alpha = 1)

# Function to read and process each combination, integrating single_method_plot logic
read_process_data <- function(params) {
  res_list <- read_objects(rds_path, params, save_method = "rds")

  per_var_data <- res_list$per_var_n
  cutoff <- round(quantile(abs(per_var_data$truth), .98), 1)

  tmp <- per_var_data %>%
    mutate(
      covered = lower <= truth & upper >= truth,
      mag_truth = abs(truth),
      covered = as.numeric(covered)
    ) %>%
    filter(n == params$n, submethod == "hybrid")

  fit <- gam(covered ~ s(mag_truth) + s(group, bs = "re"), data = tmp, family = binomial)
  xs <- seq(0, cutoff, by = .01)
  ys <- predict(fit, data.frame(mag_truth = xs, group = 101), type = "response")

  line_data <- data.frame(x = xs, y = ys, n = factor(params$n), alpha = params$nominal_coverage, modifier = params$modifier, which = "curve")
  mean_coverage <- data.frame(y = mean(tmp$covered), x = xs, n = factor(params$n), alpha = params$nominal_coverage, modifier = params$modifier, which = "mean")
  line_data <- bind_rows(line_data, mean_coverage)

  return(line_data)
}

# Apply the function across all parameter combinations and bind rows
all_data <- do.call(rbind, lapply(1:nrow(params_grid), function(i) read_process_data(params_grid[i, ])))


# Plotting with facet wrap
pdf("./fig/true_lambda.pdf", height = 3.5)
ggplot() +
  geom_line(data = all_data %>% filter(which == "curve"), aes(x = x, y = y, color = n)) +
  geom_line(data = all_data %>% filter(which == "mean"), aes(x = x, y = y, color = n), lty = "dashed") +
  facet_wrap(.~modifier) +
  geom_hline(data = all_data, aes(yintercept = 0.8), color = "black") +
  theme_bw() +
  labs(x = expression(abs(beta)), y = "Estimated Coverage Probability") +
  scale_color_manual(name = "N", values = colors) +
  coord_cartesian(ylim = c(0, 1))
dev.off()
