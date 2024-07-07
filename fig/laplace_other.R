## Setup
source("./fig/setup/setup.R")

methods <- c("selectiveinference", "lasso", "blp")
ns <- c(50, 100, 400)
data_type <- "laplace"
SNR <- 1
alpha <- .2
p <- 100
lambda <- "cv"

params_grid <- expand.grid(list(data = data_type, n = ns, snr = SNR, lambda = lambda,
                                method = methods,
                                nominal_coverage = (1 - alpha) * 100, p = p))

params_grid <- cbind(params_grid, alpha = rep(c(NA, 1, NA), each = 3))

# Fetching and combining data
per_var_data <- list()
per_dataset_data <- list()
for (i in 1:nrow(params_grid)) {
  res_list <- read_objects(rds_path, params_grid[i,], save_method = "rds")
  per_var_data[[i]] <- res_list$per_var_n
  if (params_grid$method[i] == "lasso") {
    per_var_data[[i]] <-  per_var_data[[i]] %>%
      filter(submethod %in% c("debiased", "hybrid"))
  }
  if (params_grid$method[i] %in% c("blp", "selectiveinference")) {
    per_var_data[[i]] <-  per_var_data[[i]] %>%
      rename(lower = estimate, upper = lower, estimate = upper)
  }
  per_dataset_data[[i]] <- res_list$per_dataset_n
}
per_var_data <- do.call(rbind, per_var_data) %>%
  data.frame()
per_var_data$n <- glue("({sapply(per_var_data$n, function(x) which(ns == x))}) {per_var_data$n}")
per_dataset_data <- do.call(rbind, per_dataset_data) %>%
  data.frame()
per_dataset_data$n <- glue("({sapply(per_dataset_data$n, function(x) which(ns == x))}) {per_dataset_data$n}")

n_methods <- length(unique(per_var_data$submethod))
methods <- unique(per_var_data$submethod)


## Coverage
p1 <- per_var_data %>%
  filter(!is.na(estimate)) %>%
  mutate(covered = lower <= truth & upper >= truth) %>%
  group_by(submethod, group, n) %>%
  summarise(coverage = mean(covered, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(group = glue("{submethod}-{n}")) %>%
  ggplot(aes(x = methods_pretty[submethod], y = coverage, group = group, fill = n)) +
  geom_boxplot() +
  geom_hline(yintercept = 1 - alpha) +
  scale_fill_manual(values = colors, name = "Sample Size", labels = c("50", "100", "400")) +
  ylab("Coverage") +
  xlab("Method") +
  theme_bw()

tmp <- per_var_data %>%
  filter(!is.na(estimate)) %>%
  mutate(covered = lower <= truth & upper >= truth) %>%
  group_by(submethod, group, n) %>%
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
  group_by(submethod, group, n) %>%
  summarise(width = median(width, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(group = glue("{submethod}-{n}")) %>%
  ggplot(aes(x = methods_pretty[submethod], y = width, group = group, fill = n)) +
  geom_boxplot() +
  scale_fill_manual(values = colors, name = "Sample Size", labels = c("50", "100", "400")) +
  ylab(expression(`Median Width`)) +
  xlab("Method") +
  theme_bw() +
  scale_y_continuous(labels = function(x) x, trans = "log10")

## 28, 2 failed / 4, 10, 3, infinite median
per_var_data %>%
  mutate(width = upper - lower) %>%
  group_by(submethod, group, n) %>%
  summarise(width = median(width, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(group = glue("{submethod}-{n}")) %>%
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
