## Setup
source("./fig/setup/setup.R")

methods <- c("selectiveinference", "zerosample2", "blp"); n_methods <- length(methods)
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
                                correlation_structure = corr, correlation = rho, method = methods,
                                ci_method = "quantile", nominal_coverage = alpha * 100, p = p, modifier = modifier))
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
# %>% mutate(n = factor(n, levels = ns, ordered = TRUE))
per_var_data$n <- glue("({sapply(per_var_data$n, function(x) which(ns == x))}) {per_var_data$n}")
per_dataset_data <- do.call(rbind, per_dataset_data) %>%
  data.frame()
# %>% mutate(n = factor(n, levels = ns, ordered = TRUE))
per_dataset_data$n <- glue("({sapply(per_dataset_data$n, function(x) which(ns == x))}) {per_dataset_data$n}")


## Coverage
p1 <- per_var_data %>%
  mutate(covered = lower <= truth & upper >= truth) %>%
  group_by(method, group, n) %>%
  summarise(coverage = mean(covered, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(group = glue("{method}-{n}")) %>%
  ggplot(aes(x = methods_pretty[method], y = coverage, group = group, fill = n)) +
  geom_boxplot() +
  geom_hline(yintercept = 1 - alpha) +
  scale_fill_manual(values = colors, name = "Sample Size") +
  ylab("Coverage") + xlab(NULL) +
  theme_bw()

## Time
p2 <- per_dataset_data %>%
  mutate(group = glue("{method}-{n}")) %>%
  ggplot(aes(x = methods_pretty[method], y = time, group = group, fill = n)) +
  geom_boxplot() +
  scale_fill_manual(values = colors, name = "Sample Size") +
  ylab("Time") + xlab(NULL) +
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
  scale_fill_manual(values = colors, name = "Sample Size") +
  ylab(expression(`Median Width`)) +
  xlab(NULL) +
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
  summarise(nonfinite_median = mean(is.na(width) | is.infinite(width)))

## Get summaries of times failed / infinite width / number of variables selected for si
p4 <- per_dataset_data %>%
  mutate(group = glue("{method}-{n}")) %>%
  ggplot(aes(x = methods_pretty[method], y = lambda, group = group, fill = n)) +
  geom_boxplot() +
  scale_fill_manual(values = colors, name = "Sample Size") +
  ylab(expression(lambda)) + xlab(NULL) +
  theme_bw()

glist <- list(p1, p2, p3, p4)
glist <- lapply(glist, function(x) {
  x +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 15))
})

glist[[2]] <- glist[[2]] +
  theme(legend.position = c(0.2, 0.2))

bottom_label <- textGrob("Method", gp = gpar(fontsize = 12))

suppressMessages({
  pdf("./fig/laplace_other.pdf", height = 8)
  g <- grid.arrange(grobs = glist, ncol = 2, nrow = 2, bottom = bottom_label)
  dev.off()
  if (save_rds) {
    gobj <- grid.arrange(grobs = glist, ncol = 2, nrow = 2, bottom = bottom_label)
    save(gobj, file = glue("{res_dir}/web/rds/laplace_other.rds"))
  }
})
