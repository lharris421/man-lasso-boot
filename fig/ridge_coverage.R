source("./fig/setup.R")
# Set parameters
# prior_mean <- 0
# prior_variance <- .1^2
# sigma2 <- .1^2     # Likelihood variance
# n <- 1         # Sample size
# sim <- 10000     # Number of simulations per theta
# theta_values <- seq(-.3, .3, length.out = 100)  # Range of theta values
# coverage_probs2 <- numeric(length(theta_values))  # To store coverage probabilities
# z <- qnorm(0.9)
#
# # Loop over theta values
# for (k in seq_along(theta_values)) {
#   theta_true <- theta_values[k]
#   count_in_interval <- 0
#
#   # Simulate data and calculate credible intervals
#   for (i in 1:sim) {
#     # Simulate data from the likelihood
#     Y <- rnorm(n, mean = theta_true, sd = sqrt(sigma2))
#     Y_bar <- mean(Y)
#
#     # Calculate posterior parameters
#     precision_prior <- 1 / prior_variance
#     precision_likelihood <- n / sigma2
#     precision_post <- precision_prior + precision_likelihood
#     mu_post <- (prior_mean * precision_prior + Y_bar * precision_likelihood) / precision_post
#     sigma2_post <- 1 / precision_post
#
#     # Compute the 95% credible interval
#     lower <- mu_post - z * sqrt(sigma2_post)
#     upper <- mu_post + z * sqrt(sigma2_post)
#
#     # Check if the true theta is within the credible interval
#     if (theta_true >= lower && theta_true <= upper) {
#       count_in_interval <- count_in_interval + 1
#     }
#   }
#
#   # Calculate coverage probability
#   coverage_probs2[k] <- count_in_interval / sim
# }


## Set parameters
alpha <- 0.2
prior_mean <- 0
prior_variance <- 1^2
sigma2 <- 1^2     # Likelihood variance
n <- 1         # Sample size
#theta_values <- seq(-.3, .3, length.out = 100)  # Range of theta values
 theta_values <- seq(-3, 3, length.out = 100)
z <- qnorm(1 - alpha / 2)

# Calculate prior and likelihood precisions
precision_prior <- 1 / prior_variance
precision_likelihood <- n / sigma2
precision_post <- precision_prior + precision_likelihood

# Posterior variance (for credible interval)
sigma2_post <- 1 / precision_post
sigma_post <- sqrt(sigma2_post)

# Weights
prior_weight <- precision_prior / precision_post
lik_weight <- 1 - prior_weight

# Mean difference between posterior mean and true theta
posterior_mean <- prior_weight * prior_mean + lik_weight * theta_values
sd_mu_post <- sqrt(lik_weight^2 * (sigma2 / n))

# Compute coverage probability analytically (centering at zero for ease of computation)
upper_limit <- (posterior_mean + z * sigma_post)
lower_limit <- (posterior_mean - z * sigma_post)

coverage_probs <- pnorm(theta_values, upper_limit, sd = sd_mu_post, lower.tail = FALSE) - pnorm(theta_values, lower_limit, sd = sd_mu_post, lower.tail = FALSE)

## prior dens
prior_dens <- dnorm(theta_values, sd = sqrt(prior_variance))

# Plot coverage probability vs theta
data <- data.frame(
  theta_values = theta_values,
  coverage_probs = coverage_probs,
  prior_dens_scaled = prior_dens / max(prior_dens)  # Scale prior density for plotting
)

# Calculate horizontal lines
coverage_avg <- sum(prior_dens * coverage_probs) / sum(prior_dens)
threshold <- 1 - alpha

# Plot using ggplot2
pdf("./fig/ridge_coverage.pdf", height = 4, width = 5)
ggplot(data, aes(x = theta_values)) +
  geom_line(aes(y = coverage_probs), color = "green3") +  # Line for coverage_probs
  geom_ribbon(aes(ymin = 0, ymax = prior_dens_scaled), alpha = 0.3) +  # Shaded region for prior_dens
  geom_hline(yintercept = threshold, color = "black") +  # Horizontal line at 1 - alpha
  geom_hline(yintercept = coverage_avg, color = "green3", linetype = "dashed") +  # Horizontal line for weighted avg
  labs(x = expression(theta), y = "Coverage Probability") +
  ylim(0, 1) +  # Set y-axis limits
  theme_bw()  # Minimal theme for clarity
dev.off()
