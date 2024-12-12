source("./fig/setup.R")

alpha <- 0.2
for (i in 1:length(methods)) {
  methods[[i]]$method_arguments["alpha"] <- alpha
}

simulation_info <- list(seed = 1234, iterations = 1000,
                        simulation_function = "gen_data_distribution", simulation_arguments = list(
                          p = 100, SNR = 1, sigma = 10
                        ), script_name = "distributions")

## Load data back in
methods <- methods[c("lasso_boot", "traditional")]
ns <- c(100)
distributions <- c( "laplace")

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


results <- bind_rows(results)
model_res <- calculate_model_results(results)

cutoff <- 3
line_data <- list()
line_data_avg <- list()
xvals <- seq(from = -cutoff, to = cutoff, length.out = cutoff * 100 + 1)
density_data <- data.frame(x = xvals, density = 2 * dlaplace(xvals, rate = 1.414))

methods <- unique(results$method)
for (i in 1:length(methods)) {

  cat("Processing method: ", methods[i], "\n")
  tmp <- model_res %>%
    filter(method == methods[i], !is.na(estimate))

  cat("Average coverage: ", mean(tmp$covered), "\n")

  xs <- seq(-cutoff, cutoff, by = 0.01)
  line_data[[i]] <- predict_covered(tmp, xs, methods[i])
  line_data_avg[[i]] <- data.frame(avg = mean(tmp$covered), method = methods_pretty[methods[i]])

}

line_data_avg <- do.call(rbind, line_data_avg)
line_data <- do.call(rbind, line_data)

p1 <- ggplot() +
  geom_line(data = line_data %>% mutate(method = methods_pretty[method]), aes(x = x, y = y, color = method)) +
  geom_hline(data = line_data_avg, aes(yintercept = avg, color = method), linetype = 2) +
  geom_hline(aes(yintercept = 1 - alpha), linetype = 1, alpha = .5) +
  geom_area(data = density_data, aes(x = x, y = density / max(density)), fill = "grey", alpha = 0.5) +
  theme_bw() +
  xlab(expression(abs(beta))) +
  ylab("Estimated Coverage") +
  coord_cartesian(ylim = c(0, 1)) +
  scale_color_manual(name = "Method", values = colors)


## Ridge
## Set parameters
alpha <- 0.2
prior_mean <- 0
prior_variance <- 1^2
sigma2 <- 1^2     # Likelihood variance
n <- 1         # Sample size
theta_values <- seq(-3, 3, length.out = 100)  # Range of theta values
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
p2 <- ggplot(data, aes(x = theta_values)) +
  geom_line(aes(y = coverage_probs), color = "green3") +  # Line for coverage_probs
  geom_ribbon(aes(ymin = 0, ymax = prior_dens_scaled), fill = "grey", alpha = 0.5) +  # Shaded region for prior_dens
  geom_hline(yintercept = threshold, color = "black") +  # Horizontal line at 1 - alpha
  geom_hline(yintercept = coverage_avg, color = "green3", linetype = "dashed") +  # Horizontal line for weighted avg
  labs(x = expression(theta), y = "Coverage Probability") +
  ylim(0, 1) +  # Set y-axis limits
  theme_bw()  # Minimal theme for clarity

pdf("./fig/laplace.pdf", height = 4, width = 8)
p2 + p1
dev.off()
