## Setup
source("./fig/setup/setup.R")

alpha <- .2
args_list <- list(data = "laplace",
                  snr = 1,
                  n = 100,
                  p = 100,
                  method = "lasso",
                  lambda = "across",
                  nominal_coverage = alpha * 100,
                  alpha = 1)

submethod <- "hybrid"


res_list <- read_objects(rds_path, expand.grid(args_list), save_method = "rds")
lambdas <- res_list$lambdas
true_lambdas <- res_list$true_lambdas
res <- res_list$res

pdat <- res[[1]] %>%
  filter(method == submethod) %>%
  dplyr::mutate(covered = truth >= lower & truth <= upper,
                group = as.factor(group),
                lambda = round(lambda / lambda_max, 3),
                truth = abs(truth))

lambda_cov <- pdat %>%
  group_by(lambda) %>%
  summarise(off_coverage = abs(mean(covered) - .8)) %>%
  ungroup() %>%
  arrange(off_coverage) %>%
  head(1) %>%
  pull(lambda)


# Fit a binomial model with the transformed lambda
model_cov <- gam(covered ~ te(lambda, truth), data = pdat, family = binomial)

# Create a grid for prediction on the transformed lambda scale
min_lam <- min(c(lambdas[[1]], lambda_cov))
lambda_seq <- 10^seq(log(min_lam, 10), log(max(lambdas[[1]]), 10), length.out = 100)
truth_seq <- seq(0, .275, length.out = 100)
grid <- expand.grid(lambda = lambda_seq, truth = truth_seq) %>% data.frame()

# Predict coverage probability
grid$coverage <- predict(model_cov, newdata = grid, type ="response")
grid$adjusted_coverage <- grid$coverage - 0.8

# Plot the heatmap with reversed lambda on the log10 scale
plt_cov <- ggplot(grid, aes(x = lambda, y = truth, fill = adjusted_coverage)) +
  geom_tile() +
  scale_fill_gradient2(low = "#DF536B", high = "#2297E6", mid = "white", midpoint = 0) +
  labs(y = expression(abs(beta)), fill = "Rel. Cov.", x = expression(lambda)) +
  scale_x_log10(trans = c("log10", "reverse"), breaks = breaks_log(base=10), labels = label_log(10, digits = 1)) +
  geom_vline(xintercept = mean(lambdas[[1]]), alpha = .5, col = "red") +
  geom_vline(xintercept = lambda_cov, alpha = .5, col = "blue") +
  # geom_vline(xintercept = mean(true_lambdas[[1]]), alpha = .5, col = "black") +
  theme_minimal() +
  theme(legend.title = element_text(size = 7),
        legend.text = element_text(size = 5),
        legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.6, "cm"))


png("./fig/beta_lambda_heatmap_laplace.png", height = 4, width = 5, units='in', res = 300)
plt_cov
dev.off()
