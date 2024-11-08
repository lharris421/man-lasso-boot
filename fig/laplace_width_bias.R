source("./fig/setup.R")

alpha <- 0.2
for (i in 1:length(methods)) {
  methods[[i]]$method_arguments["alpha"] <- alpha
}

simulation_info <- list(seed = 1234, iterations = 1000,
                        simulation_function = "gen_data_distribution", simulation_arguments = list(
                          p = 100, SNR = 1
                        ), script_name = "distributions")

## Load data back in
methods <- methods[c("lasso_boot", "traditional", "posterior", "lasso_posterior_pipe")]
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

trad_sign_inv <- results %>%
  filter(method == "traditional") %>%
  mutate(
    sign_inv = sign_inversion(lower, upper, truth),
    sign_inv2 = sign(upper) != sign(truth) & sign(lower) != sign(truth) & (sign(upper) != 0 | sign(lower) != 0)
  ) %>%
  # filter(sign(upper) != sign(truth) & sign(lower) != sign(truth) & (sign(upper) != 0 & sign(lower) != 0))
  filter(sign_inv2)

trad_sign_inv_ex <- trad_sign_inv %>% filter(iteration == 1) %>% pull(variable)

results %>%
  filter(iteration == 1 & variable %in% trad_sign_inv_ex)

results %>%
  filter(method == "traditional") %>%
  nrow()

plot_res <- list()
methods <- names(methods)
xs <- seq(0, 0.275, by = .01)
for (i in 1:length(methods)) {

  plot_data <- results %>%
    filter(method == methods[i]) %>%
    mutate(width = upper - lower) %>%
    mutate(mag_truth = abs(truth))

  fit <- gam(width ~ s(mag_truth), data = plot_data)
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

plot_res <- list()
for (i in 1:length(methods)) {

  plot_data <- results %>%
    filter(method == methods[i]) %>%
    mutate(
      bias_high = miss_high(lower, upper, truth),
      bias_low = miss_low(lower, upper, truth),
      bias_sign = sign_inversion(lower, upper, truth),
      covered = lower <= truth & truth <= upper,
      mag_truth = abs(truth),
      summ1 = bias_high + bias_low + bias_sign + covered
    )

  print(methods[i])
  print(plot_data %>% pull(bias_sign) %>% table())
  print(plot_data %>% pull(summ1) %>% table())

  fit <- gam(bias_high ~ s(mag_truth), data = plot_data, family = binomial)
  ys_high <- predict(fit, data.frame(mag_truth = xs, group = 101), type ="response")

  fit <- gam(bias_low ~ s(mag_truth), data = plot_data, family = binomial)
  ys_low <- predict(fit, data.frame(mag_truth = xs, group = 101), type ="response")

  fit <- gam(bias_sign ~ s(mag_truth), data = plot_data, family = binomial)
  ys_sign <- predict(fit, data.frame(mag_truth = xs, group = 101), type ="response")

  plot_res[[i]] <- data.frame(xs = xs, bias = ys_low - ys_high, bias_low = ys_low, bias_high = ys_high, bias_sign = ys_sign, method = methods_pretty[methods[i]])

}

plot_bias_h <- do.call(rbind, plot_res) %>%
  ggplot(aes(x = xs, y = bias_high, color = method)) +
  geom_line() +
  theme_bw() +
  xlab(NULL) +
  ylab("P(Away)") +
  scale_color_manual(name = "Method", values = colors) +
  theme(legend.position = "none")

plot_bias_l <- do.call(rbind, plot_res) %>%
  ggplot(aes(x = xs, y = bias_low, color = method)) +
  geom_line() +
  theme_bw() +
  xlab(NULL) +
  ylab("P(Towards)") +
  scale_color_manual(name = "Method", values = colors) +
  theme(legend.position = "none")

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

combined_plot <- (plot_width + plot_bias_s) / (plot_bias_l + plot_bias_h) +
  plot_layout(guides = "collect", axis_titles = "collect")
pdf("./fig/laplace_width_bias.pdf", height = 5, width = 8)
print(combined_plot)
dev.off()
