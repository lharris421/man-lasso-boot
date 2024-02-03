## Setup
source("./fig/setup/setup.R")

## Load Data
load(glue("{res_dir}/rds/method_comparison_laplace_100_{quantiles}_{method}_span1.rds"))
method_pretty <- c("hdi" = "BLP", "lasso_sample" = "Lasso Bootstrap", "si" = "Selective Inference")

## Other plots
p1 <- do.call(rbind, lapply(1:3, function(i) {
  t(sapply(res_coverage[[i]], function(x) apply(x[,1:3], 2, mean, na.rm = TRUE))) %>%
    data.frame() %>%
    tidyr::pivot_longer(si:lasso_sample, names_to = "method", values_to = "coverage") %>%
    mutate(group = paste0(method, "-", ns[i]), ss = as.character(ns[i]))
})) %>%
  ggplot(aes(x = method_pretty[method], y = coverage, group = group, fill = ss)) +
  geom_boxplot() +
  geom_hline(yintercept = .8) +
  scale_fill_manual(values = colors, name = "Sample Size") +
  ylab("Average Coverage") + xlab(NULL) +
  theme_bw()

do.call(rbind, lapply(1:3, function(i) {
  t(sapply(res_coverage[[i]], function(x) apply(x[,1:3], 2, function(y) sum(is.na(y))))) %>%
    data.frame() %>%
    tidyr::pivot_longer(si:lasso_sample, names_to = "method", values_to = "na") %>%
    mutate(group = paste0(method, "-", ns[i]), ss = as.character(ns[i]))
})) %>%
  filter(na > 0 & na < 30 & method == "si") %>% pull(na) %>%
  mean()

## On average provides intervals for about 10 variables

p2 <- do.call(rbind, lapply(1:3, function(i) {
  res <- res_time[[i]]
  colnames(res) <- c("si", "hdi", "lasso_sample")
  res %>%
    data.frame() %>%
    tidyr::pivot_longer(si:lasso_sample, names_to = "method", values_to = "time") %>%
    mutate(group = paste0(method, "-", ns[i]), ss = as.character(ns[i]))
})) %>%
  ggplot(aes(x = method_pretty[method], y = log10(time), group = group, fill = ss)) +
  geom_boxplot() +
  scale_fill_manual(values = colors, name = "Sample Size") +
  ylab(expression(log10(time))) + xlab(NULL) +
  theme_bw()



p3 <- do.call(rbind, lapply(1:3, function(i) {
  res <- res_width[[i]]
  colnames(res) <- c("si", "hdi", "lasso_sample")
  res %>%
    data.frame() %>%
    tidyr::pivot_longer(si:lasso_sample, names_to = "method", values_to = "width") %>%
    mutate(group = paste0(method, "-", ns[i]), ss = as.character(ns[i]))
})) %>%
  ggplot(aes(x = method_pretty[method], y = log10(width), group = group, fill = ss)) +
  geom_boxplot() +
  scale_fill_manual(values = colors, name = "Sample Size") +
  ylab(expression(log10(`Median Width`))) + xlab(NULL) +
  theme_bw()


# do.call(rbind, lapply(1:3, function(i) {
#   res <- res_width[[i]]
#   colnames(res) <- c("si", "hdi", "lasso_sample")
#   res %>%
#     data.frame() %>%
#     tidyr::pivot_longer(si:lasso_sample, names_to = "method", values_to = "width") %>%
#     mutate(group = paste0(method, "-", ns[i]), ss = as.character(ns[i]))
# })) %>%
#   filter(is.infinite(width)) %>%
#   pull(group) %>%
#   table()

## Selective inference infinite average width
## n = 20: 26, n = 30: 20, n = 60: 6


# do.call(rbind, lapply(1:3, function(i) {
#   res <- res_width[[i]]
#   colnames(res) <- c("si", "hdi", "lasso_sample")
#   res %>%
#     data.frame() %>%
#     tidyr::pivot_longer(si:lasso_sample, names_to = "method", values_to = "width") %>%
#     mutate(group = paste0(method, "-", ns[i]), ss = as.character(ns[i]))
# })) %>%
#   filter(is.na(width)) %>%
#   pull(group) %>%
#   table()

## Method failed
## n = 20: BLP = 2, SI = 4
## n = 30: SI: 1




## Get summaries of times failed / infinite width / number of variables selected for si

p4 <- do.call(rbind, lapply(1:3, function(i) {
  res <- res_lambda[[i]]
  colnames(res) <- c("si", "hdi", "lasso_sample")
  res %>%
    data.frame() %>%
    tidyr::pivot_longer(si:lasso_sample, names_to = "method", values_to = "lambda") %>%
    mutate(group = paste0(method, "-", ns[i]), ss = as.character(ns[i]))
})) %>%
  ggplot(aes(x = method_pretty[method], y = lambda, group = group, fill = ss)) +
  geom_boxplot() +
  scale_fill_manual(values = colors, name = "Sample Size") +
  ylab(expression(lambda)) + xlab(NULL) +
  theme_bw()


glist <- list(p1, p2, p3, p4)
glist <- lapply(glist, function(x) {
  x +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 7.5))
})

glist[[2]] <- glist[[2]] +
  theme(legend.position = c(0.2, 0.2))

bottom_label <- textGrob("Method", gp = gpar(fontsize = 12))

suppressMessages({
  pdf("./fig/method_comparison_laplace_100_other.pdf", height = 8)
  g <- grid.arrange(grobs = glist, ncol = 2, nrow = 2, bottom = bottom_label)
  dev.off()
  png("./fig/method_comparison_laplace_100_other.png", width = 600, height = 700)
  grid.arrange(grobs = glist, ncol = 2, nrow = 2, bottom = bottom_label)
  dev.off()
})
