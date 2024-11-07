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
methods <- methods[c("lasso_boot")]
ns <- c(50, 100, 400)
distributions <- c( "laplace")
true_lambda <- TRUE
true_sigma2 <- c(FALSE, TRUE)

files <- expand.grid(
  "method" = names(methods),
  "n" = ns,
  "distribution" = distributions,
  "true_lambda" = true_lambda,
  "true_sigma2" = true_sigma2,
  stringsAsFactors = FALSE
)

results <- list()
for (i in 1:nrow(files)) {

  simulation_info$simulation_arguments$n <- files[i,] %>% pull(n)
  simulation_info$simulation_arguments$distribution <- files[i,] %>% pull(distribution)
  simulation_info$true_lambda <- files[i,] %>% pull(true_lambda)
  simulation_info$true_sigma2 <- files[i,] %>% pull(true_sigma2)

  tmp <- indexr::read_objects(
    rds_path,
    c(methods[[files[i,"method"]]], simulation_info)
    # args
  ) %>%
    mutate(method = files[i,] %>% pull(method), distribution = files[i,] %>% pull(distribution), n = files[i,] %>% pull(n))

  cutoff <- round(quantile(abs(tmp$truth), .98), 1)

  tmp <- tmp %>%
    mutate(
      covered = lower <= truth & upper >= truth,
      mag_truth = abs(truth),
      covered = as.numeric(covered)
    )

  fit <- gam(covered ~ s(mag_truth), data = tmp, family = binomial)
  xs <- seq(0, cutoff, by = .01)
  ys <- predict(fit, data.frame(mag_truth = xs, group = 101), type = "response")

  line_data <- data.frame(x = xs, y = ys, n = factor(simulation_info$simulation_arguments$n), which = "curve")
  mean_coverage <- data.frame(y = mean(tmp$covered), x = xs, n = factor(simulation_info$simulation_arguments$n), which = "mean")
  line_data <- bind_rows(line_data, mean_coverage)

  results[[i]] <- line_data %>%
    mutate(group = ifelse(simulation_info$true_sigma2, "True Lambda / Sigma", "True Lambda"))
}

all_data <- bind_rows(results)

# Plotting with facet wrap
pdf("./fig/true_lambda.pdf", height = 3.5)
  ggplot() +
    geom_line(data = all_data %>% filter(which == "curve"), aes(x = x, y = y, color = n)) +
    geom_line(data = all_data %>% filter(which == "mean"), aes(x = x, y = y, color = n), lty = "dashed") +
    facet_wrap(.~group) +
    geom_hline(data = all_data, aes(yintercept = 1 - alpha), color = "black") +
    theme_bw() +
    labs(x = expression(abs(beta)), y = "Estimated Coverage Probability") +
    scale_color_manual(name = "N", values = colors) +
    coord_cartesian(ylim = c(0, 1))
dev.off()

