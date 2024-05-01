## Setup
source("./fig/setup/setup.R")

## Load Data
cutoff <- 0.275
xs <- seq(0, cutoff, by = .01)


methods <- c("sample", "zerosample2", "debiased", "traditional")
n <- 100
data_type <- "laplace"
SNR <- 1
alpha <- .2
p <- 100

params_grid <- expand.grid(list(data = data_type, n = n, snr = SNR,
                                method = methods, lambda = "cv",
                                ci_method = "quantile", nominal_coverage = alpha * 100, p = p))


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
for (i in 1:length(methods)) {
  plot_data <- per_var_data %>%
    filter(method == methods[i]) %>%
    mutate(width = upper - lower) %>%
    filter(!is.na(width) & is.finite(width)) %>%
    mutate(mag_truth = abs(truth))
  print(methods[i])
  fit <- gam(width ~ s(mag_truth) + s(group, bs = "re"), data = plot_data)
  ys <- predict(fit, data.frame(mag_truth = xs, group = 101), type ="response")
  plot_res[[i]] <- data.frame(xs = xs, width = ys, method = methods_pretty[methods[i]])
}

plot_width <- do.call(rbind, plot_res) %>%
  ggplot(aes(x = xs, y = width, color = method)) +
  geom_line() +
  theme_bw() +
  xlab(NULL) +
  ylab(NULL) +
  scale_color_manual(name = "Method", values = colors)

## Center
bias_est <- function(center, truth) {
  case_when(
    sign(truth) == -1 ~ center - truth,
    sign(truth) == 1 ~ truth - center
  )
}

xs <- seq(0, cutoff, by = .01)
plot_res <- list()
for (i in 1:length(methods)) {

  plot_data <- per_var_data %>%
    filter(method == methods[i]) %>%
    rowwise() %>%
    mutate(bias = bias_est(center, truth)) %>%
    filter(!is.na(bias) & is.finite(bias)) %>%
    mutate(mag_truth = abs(truth))

  print(methods[i])
  fit <- gam(bias ~ s(mag_truth) + s(group, bs = "re"), data = plot_data)
  ys <- predict(fit, data.frame(mag_truth = xs, group = 101), type ="response")
  plot_res[[i]] <- data.frame(xs = xs, bias = ys, method = methods_pretty[methods[i]])
}

plot_bias <- do.call(rbind, plot_res) %>%
  ggplot(aes(x = xs, y = bias, color = method)) +
  geom_line() +
  theme_bw() +
  xlab(NULL) +
  ylab(NULL) +
  scale_color_manual(name = "Method", values = colors)

## lhb
miss_low <- function(lower, upper, truth) {
  case_when(
    lower <= truth & upper >= truth ~ 0,
    sign(truth) == 1 & sign(upper) %in% c(0, 1) & upper < truth  ~ 1,
    sign(truth) == -1 & sign(lower) %in% c(-1, 0) & lower > truth ~ 1,
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
    sign(truth) == 1 & sign(upper) == -1 ~ 1,
    sign(truth) == -1 & sign(lower) == 1 ~ 1,
    TRUE ~ 0
  )
}

xs <- seq(0, cutoff, by = .01)
plot_res <- list()
for (i in 1:length(methods)) {

  plot_data <- per_var_data %>%
    filter(method == methods[i]) %>%
    rowwise() %>%
    mutate(bias_high = miss_high(lower, upper, truth)) %>%
    mutate(bias_low = miss_low(lower, upper, truth)) %>%
    mutate(bias_sign = sign_inversion(lower, upper, truth)) %>%
    mutate(mag_truth = abs(truth))

  print(methods[i])
  fit <- gam(bias_high ~ s(mag_truth) + s(group, bs = "re"), data = plot_data, family = binomial)
  ys_high <- predict(fit, data.frame(mag_truth = xs, group = 101), type ="response")
  fit <- gam(bias_low ~ s(mag_truth) + s(group, bs = "re"), data = plot_data, family = binomial)
  ys_low <- predict(fit, data.frame(mag_truth = xs, group = 101), type ="response")
  fit <- gam(bias_sign ~ s(mag_truth) + s(group, bs = "re"), data = plot_data, family = binomial)
  ys_sign <- predict(fit, data.frame(mag_truth = xs, group = 101), type ="response")

  plot_res[[i]] <- data.frame(xs = xs, bias = ys_low - ys_high, bias_low = ys_low, bias_high = ys_high, bias_sign = ys_sign, method = methods_pretty[methods[i]])
}

plot_bias_hl <- do.call(rbind, plot_res) %>%
  ggplot(aes(x = xs, y = bias, color = method)) +
  geom_line() +
  ggtitle("Towards Zero - Far From Zero Balance") +
  theme_bw() +
  xlab(expression(abs(beta))) +
  ylab("P(Event)") +
  scale_color_manual(name = "Method", values = colors)

plot_bias_h <- do.call(rbind, plot_res) %>%
  ggplot(aes(x = xs, y = bias_high, color = method)) +
  geom_line() +
  ggtitle("Far From Zero") +
  theme_bw() +
  xlab(NULL) +
  ylab("P(Event)") +
  scale_color_manual(name = "Method", values = colors) +
  theme(legend.position = "none")

plot_bias_l <- do.call(rbind, plot_res) %>%
  ggplot(aes(x = xs, y = bias_low, color = method)) +
  geom_line() +
  ggtitle("Towards Zero") +
  theme_bw() +
  xlab(NULL) +
  ylab("P(Event)") +
  scale_color_manual(name = "Method", values = colors) +
  theme(legend.position = "none")

plot_bias_s <- do.call(rbind, plot_res) %>%
  ggplot(aes(x = xs, y = bias_sign, color = method)) +
  geom_line() +
  ggtitle("Sign Inversion") +
  theme_bw() +
  xlab(expression(abs(beta))) +
  ylab("P(Inversion)") +
  scale_color_manual(name = "Method", values = colors)

# Create a layout matrix
combined_plot <- (((plot_bias_l + plot_bias_h) +
  plot_layout(axis_titles = "collect")) /
  plot_bias_hl) +
  plot_layout(guides = "collect")
pdf("./fig/laplace_bias_nfb.pdf", height = 4, width = 8)
print(combined_plot)
dev.off()

pdf("./fig/laplace_bias_sign.pdf", height = 4, width = 8)
print(plot_bias_s)
dev.off()


plot_width <- plot_width +
  xlab(expression(abs(beta))) +
  ylab("Interval Width") +
  coord_cartesian(ylim = c(0, 0.35))

plot_bias <- plot_bias +
  xlab(expression(abs(beta))) +
  ylab("Bias") +
  coord_cartesian(ylim = c(0, 0.35))

library(cowplot)
library(patchwork)
combined_plot <- plot_width + plot_bias +
  plot_layout(guides = "collect", axis_titles = "collect")
pdf("./fig/laplace_width_bias.pdf", height = 3, width = 8)
print(combined_plot)
dev.off()
