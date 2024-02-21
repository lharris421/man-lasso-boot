## Setup
source("./fig/setup/setup.R")

# methods <- c("selective_inference", "zerosample2", "blp")
n_methods <- length(methods)
methods <- c("traditional", "sample", "debiased", "zerosample2")
methods <- c("zerosample2")
methods <- c("truncatedzs2")
n_methods <- length(methods)

data_type <- "laplace"

rt <- 2
SNR <- 1

corr <- "exchangeable"
rho <- 0

# corr <- "autoregressive"
# rho <- .7

per_var_data <- list()
alpha <- .2
p <- 100

per_var_data <- list()
per_dataset_data <- list()
for (i in 1:n_methods) {
  # load(glue("{res_dir}/rds/laplace_{methods[i]}.rds"))
  ad_inf <- ifelse(data_type == "laplace", rt, a)
  load(glue("{res_dir}/rds/{data_type}({ad_inf})_SNR{SNR}_{corr}_rho{rho*100}_{methods[i]}_alpha{alpha*100}_p{p}.rds"))
  per_var_data[[i]] <- per_var
  per_dataset_data[[i]] <- per_dataset
}
per_var_data <- do.call(rbind, per_var_data) %>%
  data.frame()
per_dataset_data <- do.call(rbind, per_dataset_data) %>%
  data.frame()
ns <- unique(per_var_data$n)

## Coverage
p1 <- per_var_data %>%
  mutate(covered = lower <= truth & upper >= truth) %>%
  group_by(method, group, n) %>%
  summarise(coverage = mean(covered, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(group = glue("{method}-{n}"), n = as.factor(n)) %>%
  ggplot(aes(x = methods_pretty[method], y = coverage, group = group, fill = n)) +
  geom_boxplot() +
  geom_hline(yintercept = 1 - alpha) +
  scale_fill_manual(values = colors, name = "Sample Size") +
  ylab("Average Coverage") + xlab(NULL) +
  theme_bw()

## Time
p2 <- per_dataset_data %>%
  mutate(group = glue("{method}-{n}"), n = as.factor(n)) %>%
  ggplot(aes(x = methods_pretty[method], y = log10(time), group = group, fill = n)) +
  geom_boxplot() +
  scale_fill_manual(values = colors, name = "Sample Size") +
  ylab(expression(log10(time))) + xlab(NULL) +
  theme_bw()

p3 <- per_var_data %>%
  mutate(width = upper - lower) %>%
  group_by(method, group, n) %>%
  summarise(width = median(width, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(group = glue("{method}-{n}"), n = as.factor(n)) %>%
  # ggplot(aes(x = methods_pretty[method], y = log10(width), group = group, fill = n)) +
  ggplot(aes(x = methods_pretty[method], y = width, group = group, fill = n)) +
  geom_boxplot() +
  scale_fill_manual(values = colors, name = "Sample Size") +
  # ylab(expression(log10(`Median Width`))) +
  ylab(expression(`Median Width`)) +
  xlab(NULL) +
  theme_bw()

## 28, 2 failed / 4, 10, 3, infinite median
per_var_data %>%
  mutate(width = upper - lower) %>%
  group_by(method, group, n) %>%
  summarise(width = median(width, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(group = glue("{method}-{n}"), n = as.factor(n)) %>%
  group_by(group) %>%
  summarise(nonfinite_median = mean(is.na(width) | is.infinite(width)))

## Get summaries of times failed / infinite width / number of variables selected for si
p4 <- per_dataset_data %>%
  mutate(group = glue("{method}-{n}"), n = as.factor(n)) %>%
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
