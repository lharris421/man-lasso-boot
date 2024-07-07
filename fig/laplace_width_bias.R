## Setup
source("./fig/setup/setup.R")

## Load Data
cutoff <- 0.275
xs <- seq(0, cutoff, by = .01)


methods <- c("lasso")
n_values <- 100
data_type <- "laplace"
SNR <- 1
alpha <- .2
p <- 100
modifier <- NA
enet_alpha <- 1
gamma <- NA

params_grid <- expand.grid(list(data = data_type, n = n_values, snr = SNR,
                                method = methods, lambda = "cv", alpha = enet_alpha, gamma = gamma,
                                nominal_coverage = (1-alpha) * 100, p = p, modifier = modifier))


# Fetching and combining data
per_var_data <- list()
for (i in 1:nrow(params_grid)) {
  res_list <- read_objects(rds_path, params_grid[i,], save_method = "rds")
  per_dataset_n <- res_list$per_dataset_n
  per_var_n <- res_list$per_var_n
  per_var_data[[i]] <- per_var_n
}
per_var_data <- do.call(rbind, per_var_data) %>%
  data.frame()


plot_res <- list()
submethods <- c("traditional", "posterior", "hybrid", "debiased")
for (i in 1:length(submethods)) {
  plot_data <- per_var_data %>%
    filter(submethod == submethods[i]) %>%
    mutate(width = upper - lower) %>%
    filter(!is.na(width) & is.finite(width)) %>%
    mutate(mag_truth = abs(truth))
  fit <- gam(width ~ s(mag_truth) + s(group, bs = "re"), data = plot_data)
  ys <- predict(fit, data.frame(mag_truth = xs, group = 101), type ="response")
  plot_res[[i]] <- data.frame(xs = xs, width = ys, method = methods_pretty[submethods[i]])
}

plot_width <- do.call(rbind, plot_res) %>%
  ggplot(aes(x = xs, y = width, color = method)) +
  geom_line() +
  theme_bw() +
  xlab(NULL) +
  ylab(NULL) +
  scale_color_manual(name = "Method", values = colors)


## lhb
miss_low <- function(lower, upper, truth) {
  case_when(
    lower <= truth & upper >= truth ~ 0,
    sign(truth) == 0 ~ 0,
    sign(truth) == 1 & sign(upper) == 1 & upper < truth  ~ 1,
    sign(truth) == -1 & sign(lower) == -1 & lower > truth ~ 1,
    sign(truth) != 0 & sign(lower) == 0 & sign(upper) == 0 ~ 1,
    TRUE ~ 0
  )
}
miss_high <- function(lower, upper, truth) {
  case_when(
    lower <= truth & upper >= truth ~ 0,
    sign(truth) == 1 & lower > truth ~ 1,
    sign(truth) == -1 & upper < truth ~ 1,
    TRUE ~ 0
  )
}
sign_inversion <- function(lower, upper, truth) {
  case_when(
    lower <= truth & upper >= truth ~ 0,
    (sign(truth) == 1 & sign(upper) == -1) | (sign(truth) == 1 & sign(upper) == 0 & sign(lower) == -1) ~ 1,
    (sign(truth) == -1 & sign(lower) == 1) | (sign(truth) == -1 & sign(lower) == 0 & sign(upper) == 1) ~ 1,
    TRUE ~ 0
  )
}

xs <- seq(0, cutoff, by = .01)
plot_res <- list()
for (i in 1:length(submethods)) {

  plot_data <- per_var_data %>%
    filter(submethod == submethods[i]) %>%
    rowwise() %>%
    mutate(bias_high = miss_high(lower, upper, truth)) %>%
    mutate(bias_low = miss_low(lower, upper, truth)) %>%
    mutate(bias_sign = sign_inversion(lower, upper, truth)) %>%
    mutate(bias_lowsign = miss_low(lower, upper, truth) + sign_inversion(lower, upper, truth)) %>%
    mutate(mag_truth = abs(truth))

  fit <- gam(bias_high ~ s(mag_truth) + s(group, bs = "re"), data = plot_data, family = binomial)
  ys_high <- predict(fit, data.frame(mag_truth = xs, group = 101), type ="response")
  fit <- gam(bias_low ~ s(mag_truth) + s(group, bs = "re"), data = plot_data, family = binomial)
  ys_low <- predict(fit, data.frame(mag_truth = xs, group = 101), type ="response")
  fit <- gam(bias_sign ~ s(mag_truth) + s(group, bs = "re"), data = plot_data, family = binomial)
  ys_sign <- predict(fit, data.frame(mag_truth = xs, group = 101), type ="response")
  fit <- gam(bias_lowsign ~ s(mag_truth) + s(group, bs = "re"), data = plot_data, family = binomial)
  ys_lowsign <- predict(fit, data.frame(mag_truth = xs, group = 101), type ="response")

  plot_res[[i]] <- data.frame(xs = xs, bias = ys_low - ys_high, bias_low = ys_low, bias_high = ys_high, bias_sign = ys_sign, bias_lowsign = ys_lowsign, method = methods_pretty[submethods[i]])
}

plot_bias_h <- do.call(rbind, plot_res) %>%
  ggplot(aes(x = xs, y = bias_high, color = method)) +
  geom_line() +
  theme_bw() +
  xlab(NULL) +
  ylab("P(Away)") +
  scale_color_manual(name = "Method", values = colors) +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0.1)

plot_bias_l <- do.call(rbind, plot_res) %>%
  ggplot(aes(x = xs, y = bias_low, color = method)) +
  geom_line() +
  theme_bw() +
  xlab(NULL) +
  ylab("P(Towards)") +
  scale_color_manual(name = "Method", values = colors) +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0.1)

plot_bias_ls <- do.call(rbind, plot_res) %>%
  ggplot(aes(x = xs, y = bias_lowsign, color = method)) +
  geom_line() +
  theme_bw() +
  xlab(NULL) +
  ylab("P(Towards | Type 3)") +
  scale_color_manual(name = "Method", values = colors) +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0.1)

plot_bias_s <- do.call(rbind, plot_res) %>%
  ggplot(aes(x = xs, y = bias_sign, color = method)) +
  geom_line() +
  theme_bw() +
  xlab(expression(abs(beta))) +
  ylab("P(Type 3 Error)") +
  scale_color_manual(name = "Method", values = colors)

plot_width <- plot_width +
  xlab(expression(abs(beta))) +
  ylab("Interval Width") +
  coord_cartesian(ylim = c(0, 0.35))

combined_plot <- (plot_width + plot_bias_s) / (plot_bias_ls + plot_bias_h) +
  plot_layout(guides = "collect", axis_titles = "collect")
pdf("./fig/laplace_width_bias.pdf", height = 5, width = 8)
print(combined_plot)
dev.off()
