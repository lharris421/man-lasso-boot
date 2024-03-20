## Setup
source("./fig/setup/setup.R")

alpha <- .2
base_params <- list(data = "laplace",
                    rate = 2,
                    snr = 1,
                    n = 100,
                    p = 100,
                    correlation_structure = "exchangeable",
                    correlation = 0,
                    correlation_noise = NA,
                    method = "zerosample2",
                    ci_method = "quantile",
                    lambda = "across",
                    nominal_coverage = alpha * 100)

read_objects(rds_path, expand.grid(base_params))

lambda_max <- 1
lambda_min <- 0.001
lambda_seq <- 10^(seq(log(lambda_max, 10), log(lambda_min, 10), length.out = 10))

pdat <- res[[1]] %>%
  dplyr::filter(lambda_ind <= 10) %>%
  dplyr::mutate(covered = truth >= lower & truth <= upper,
                width = upper - lower,
                group = as.factor(group),
                lambda = lambda_seq[lambda_ind],
                truth = abs(truth))

pdat %>%
  group_by(lambda) %>%
  summarise(coverage = mean(covered))


# Fit a binomial model with the transformed lambda
# model <- gam(covered ~ te(lambda, truth) + s(group, bs = "re"), data = pdat)
model_cov <- mgcv::gam(covered ~ te(lambda, truth), data = pdat, family = binomial)
model_width <- mgcv::gam(width ~ te(lambda, truth), data = pdat)

# Create a grid for prediction on the transformed lambda scale
lambda_seq <- 10^seq(log(1, 10), log(0.001, 10), length.out = 100)
truth_seq <- seq(0, 2, length.out = 100)
grid <- expand.grid(lambda = lambda_seq, truth = truth_seq) %>% data.frame()

# Predict coverage probability
grid$coverage <- predict(model_cov, newdata = grid, type ="response")
grid$adjusted_coverage <- grid$coverage - 0.8
grid$width <- predict(model_width, newdata = grid, type ="response")

## Average coverage
# av_grid <- data.frame(expand.grid(lambda = mean(lambdas[[1]]), truth = pdat$truth))
# av_grid$coverage <- predict(model, newdata = av_grid, type ="response")
# mean(av_grid$coverage)

# Plot the heatmap with reversed lambda on the log10 scale
plt_cov <- ggplot(grid, aes(x = lambda, y = truth, fill = adjusted_coverage)) +
  geom_tile() +
  scale_fill_gradient2(low = "#DF536B", high = "#2297E6", mid = "white", midpoint = 0) +
  labs(y = "Truth", fill = "Rel. Cov.", x = expression(lambda)) +
  theme_minimal() +
  scale_x_log10(trans = c("log10", "reverse"), breaks = breaks_log(base=10), labels = label_log(10)) +
  #coord_cartesian(xlim = c(max(lambdas[[1]]), min(lambdas[[1]])))  +
  # geom_vline(xintercept = lambdas[[1]], alpha = .1) +
  geom_vline(xintercept = mean(lambdas[[1]]), alpha = .5, col = "red") +
  theme(legend.title = element_text(size = 7),
        legend.text = element_text(size = 5),
        legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.6, "cm"))
  # geom_vline(xintercept = lambdas[[1]], alpha = .1) +

# Plot the heatmap with reversed lambda on the log10 scale
plt_width <- ggplot(grid, aes(x = lambda, y = truth, fill = width)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 1) +
  labs(y = "Truth", fill = "Width", x = expression(lambda)) +
  theme_minimal() +
  scale_x_continuous(trans = log10_trans(),
                     breaks = trans_breaks('log10', function(x) 10^x),
                     labels = trans_format('log10', math_format(10^.x))) +
  coord_cartesian(xlim = c(1, .001)) +
  theme(legend.title = element_text(size = 9),
        legend.text = element_text(size = 7),
        legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.6, "cm"))

plt_overall <- pdat %>%
  group_by(lambda) %>%
  summarise(coverage = mean(covered)) %>%
  ggplot(aes(x = lambda, y = coverage)) +
  geom_line() +
  theme_minimal() +
  scale_x_continuous(trans = log10_trans(),
                     breaks = trans_breaks('log10', function(x) 10^x),
                     labels = trans_format('log10', math_format(10^.x))) +
  coord_cartesian(xlim = c(1, .001)) +
  labs(y = "Overall Coverage", x = expression(lambda)) +
  geom_hline(yintercept = .8, col = "red", lty = 2)


# suppressMessages({
  pdf("./fig/beta_lambda_heatmap_laplace.pdf", height = 4, width = 5)
  # grid.arrange(grobs = list(plt_cov, plt_width, plt_overall), nrow = 2,
  #              layout_matrix = rbind(c(1, 3), c(2, 3)),
  #              widths = c(100, 50))
  plt_cov
  # grid.arrange(grobs = list(plt_cov, plt_overall), nrow = 1,
  #              widths = c(60, 40))
  dev.off()
  if (save_rds) save(plt, file = glue("{res_dir}/web/rds/beta_lambda_heatmap_laplace.rds"))
# })
