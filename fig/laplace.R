## Setup
source("./fig/setup/setup.R")

plots <- list()

methods <- c("sample", "zerosample2", "debiased", "traditional")
n_values <- c(100) # ns values you are interested in
data_type <- "laplace"
SNR <- 1
alpha <- .2
p <- 100
modifier <- NA

params_grid <- expand.grid(list(data = data_type, n = n_values, snr = SNR,
                    method = methods, lambda = "cv",
                    ci_method = "quantile", nominal_coverage = alpha * 100, p = p, modifier = modifier))


# Fetching and combining data
per_var_data <- list()
for (i in 1:nrow(params_grid)) {
  res_list <- read_objects(rds_path, params_grid[i,], save_method = "rds")
  per_dataset_n <- res_list$per_dataset_n
  per_var_n <- res_list$per_var_n
  print(unique(per_var_n$n))
  per_var_data[[i]] <- per_var_n
}
per_var_data <- do.call(rbind, per_var_data) %>%
  data.frame()
methods[1] <- "sample"


cutoff <- 0.275
ns <- unique(per_var_data$n)

# Function to calculate model results
calculate_model_results <- function(data) {
  data %>%
    mutate(
      covered = lower <= truth & upper >= truth,
      mag_truth = abs(truth),
      covered = as.numeric(covered)
    )
}

# Function to perform fitting and prediction
predict_covered <- function(data, x_values, method) {
  fit <- gam(covered ~ s(mag_truth) + s(group, bs = "re"), data = data, family = binomial)
  y_values <- predict(fit, data.frame(mag_truth = x_values, group = 101), type = "response")
  data.frame(x = x_values, y = y_values, method = method)
}

plots <- vector("list", length(ns))

for (j in seq_along(ns)) {
  model_res <- calculate_model_results(per_var_data)

  line_data <- list()
  line_data_avg <- list()
  xvals <- seq(from = 0, to = cutoff, length.out = cutoff * 100 + 1)
  density_data <- data.frame(x = xvals, density = 2 * dlaplace(xvals, rate = 14.14))

  for (i in seq_along(methods)) {
    cat("Processing method: ", methods[i], "\n")
    tmp <- model_res %>%
      filter(method == methods[i], n == ns[j], !is.na(estimate))

    cat("Average coverage: ", mean(tmp$covered), "\n")

    xs <- seq(0, cutoff, by = 0.01)
    line_data[[i]] <- predict_covered(tmp, xs, methods[i])
    line_data_avg[[i]] <- data.frame(avg = mean(tmp$covered), method = methods_pretty[methods[i]])
  }

  line_data_avg <- do.call(rbind, line_data_avg)
  line_data <- do.call(rbind, line_data)

  plots[[j]] <- ggplot() +
    geom_line(data = line_data %>% mutate(method = methods_pretty[method]), aes(x = x, y = y, color = method)) +
    geom_hline(data = line_data_avg, aes(yintercept = avg, color = method), linetype = 2) +
    geom_hline(aes(yintercept = 1 - alpha), linetype = 1, alpha = .5) +
    geom_area(data = density_data, aes(x = x, y = density / max(density)), fill = "grey", alpha = 0.5) +
    theme_bw() +
    xlab(expression(abs(beta))) +
    ylab("Coverage") +
    coord_cartesian(ylim = c(0, 1)) +
    scale_color_manual(name = "Method", values = colors)
}

pdf("./fig/laplace.pdf", height = 4, width = 8)
plots[[1]]
dev.off()
