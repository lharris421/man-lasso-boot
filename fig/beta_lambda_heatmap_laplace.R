## Setup
source("./fig/setup/setup.R")

## Load Data
quantiles <- "zerosample2"
load(glue("{res_dir}/rds/across_lambda_coverage_laplace_{quantiles}.rds"))

pdat <- plot_res[[2]]$plot_data %>%
  dplyr::mutate(covered = truth >= lower & truth <= upper,
                group = as.factor(lambda),
                truth = abs(truth))


# Fit a binomial model with the transformed lambda
# model <- glmer(covered ~ lambda * truth + (1|group), family = binomial, data = pdat)
model <- gam(covered ~ te(lambda, truth) + s(group, bs = "re"), data = pdat)

# Create a grid for prediction on the transformed lambda scale
lambda_seq <- 10^seq(min(log10(pdat$lambda)), max(log10(pdat$lambda)), length.out = 50)
truth_seq <- seq(min(pdat$truth), max(pdat$truth), length.out = 50)
grid <- expand.grid(lambda = lambda_seq, truth = truth_seq)
grid <- data.frame(grid) %>%
  mutate(group = as.character(lambda))

# Predict coverage probability
# grid$coverage <- predict(model, newdata = grid, type = "response", allow.new.levels = TRUE)
grid$coverage <- predict(model, newdata = grid, type ="response")
grid$adjusted_coverage <- grid$coverage - 0.8


# Plot the heatmap with reversed lambda on the log10 scale
plt <- ggplot(grid, aes(x = lambda, y = truth, fill = adjusted_coverage)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(y = "Truth", fill = "Adjusted Coverage") +
  theme_minimal() +
  scale_x_continuous(trans = log10_trans(),
                     breaks = trans_breaks('log10', function(x) 10^x),
                     labels = trans_format('log10', math_format(10^.x))) +
  coord_cartesian(xlim = c(10^(.55), 10^(-2.55)))


# suppressMessages({
  pdf("./fig/beta_lambda_heatmap_laplace.pdf", height = 4.5)
  plt
  dev.off()
  if (save_rds) save(plt, file = glue("{res_dir}/web/rds/beta_lambda_heatmap_laplace.rds"))
# })
