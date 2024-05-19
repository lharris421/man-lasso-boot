## Setup
source("./fig/setup/setup.R")

ns <- c(50, 100, 400) # ns values you are interested in
data_type <- "laplace"
rate <- 2
SNR <- 1
corr <- "exchangeable"
rho <- 0
alpha <- .2
p <- 100
modifier <- NA
lambda <- "cv"

params_grid <- expand.grid(list(data = data_type, n = ns, rate = rate, snr = SNR, lambda = lambda,
                                correlation_structure = corr, correlation = rho, method = c("selectiveinference", "zerosample2", "blp"),
                                ci_method = "quantile", nominal_coverage = alpha * 100, p = p, modifier = modifier))

methods <- c("selectiveinference", "zerosample2", "blp"); n_methods <- length(methods)

# Fetching and combining data
per_var_data <- list()
per_dataset_data <- list()
for (i in 1:nrow(params_grid)) {
  read_objects(rds_path, params_grid[i,])
  per_var_data[[i]] <- per_var_n
  per_dataset_data[[i]] <- per_dataset_n
}

per_var_data <- do.call(rbind, per_var_data) %>%
  data.frame()
per_var_data$n <- glue("({sapply(per_var_data$n, function(x) which(ns == x))}) {per_var_data$n}")
per_dataset_data <- do.call(rbind, per_dataset_data) %>%
  data.frame()
per_dataset_data$n <- glue("({sapply(per_dataset_data$n, function(x) which(ns == x))}) {per_dataset_data$n}")


## Coverage
p1 <- per_var_data %>%
  filter(!is.na(estimate)) %>%
  mutate(covered = lower <= truth & upper >= truth) %>%
  group_by(method, group, n) %>%
  summarise(coverage = mean(covered, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(group = glue("{method}-{n}")) %>%
  ggplot(aes(x = methods_pretty[method], y = coverage, group = group, fill = n)) +
  geom_boxplot() +
  geom_hline(yintercept = 1 - alpha) +
  scale_fill_manual(values = colors, name = "Sample Size", labels = c("50", "100", "400")) +
  ylab("Coverage") +
  xlab("Method") +
  theme_bw()

tmp <- per_var_data %>%
  filter(!is.na(estimate)) %>%
  mutate(covered = lower <= truth & upper >= truth) %>%
  group_by(method, group, n) %>%
  summarise(coverage = mean(covered, na.rm = TRUE))

## Time
p2 <- per_dataset_data %>%
  mutate(group = glue("{method}-{n}")) %>%
  ggplot(aes(x = methods_pretty[method], y = time, group = group, fill = n)) +
  geom_boxplot() +
  scale_fill_manual(values = colors, name = "Sample Size", labels = c("50", "100", "400")) +
  ylab("Time") +
  xlab("Method") +
  theme_bw() +
  scale_y_continuous(labels = function(x) x, trans = "log10")

p3 <- per_var_data %>%
  mutate(width = upper - lower) %>%
  group_by(method, group, n) %>%
  summarise(width = median(width, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(group = glue("{method}-{n}")) %>%
  ggplot(aes(x = methods_pretty[method], y = width, group = group, fill = n)) +
  geom_boxplot() +
  scale_fill_manual(values = colors, name = "Sample Size", labels = c("50", "100", "400")) +
  ylab(expression(`Median Width`)) +
  xlab("Method") +
  theme_bw() +
  scale_y_continuous(labels = function(x) x, trans = "log10")

## 28, 2 failed / 4, 10, 3, infinite median
per_var_data %>%
  mutate(width = upper - lower) %>%
  group_by(method, group, n) %>%
  summarise(width = median(width, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(group = glue("{method}-{n}")) %>%
  group_by(group) %>%
  summarise(nonfinite_median = mean(is.infinite(width)))

per_var_data %>%
  filter(method == "selectiveinference") %>%
  mutate(width = upper - lower) %>%
  group_by(method, group, n) %>%
  summarise(width = any(is.infinite(width), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(group = glue("{method}-{n}")) %>%
  group_by(group) %>%
  summarise(nonfinite_median = sum(width))


pdf("./fig/laplace_other.pdf", height = 5, width = 7)
(p1 / p3 / p2) + plot_layout(guides = "collect", axes = "collect")
dev.off()
