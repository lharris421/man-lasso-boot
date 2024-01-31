## Setup
source("./fig/setup/setup.R")

## Load Data
method <- "bucketfill"

load(glue("{res_dir}/rds/lassoboot_comparison_laplace_100_{method}.rds"))

## Other plots
p1 <- do.call(rbind, lapply(1:3, function(i) {
  t(sapply(res_coverage[[i]], function(x) apply(x[,3:8], 2, mean, na.rm = TRUE))) %>%
   # do.call(rbind, lapply(res_coverage[[i]], function(x) apply(x[,3,drop=FALSE], 2, mean, na.rm = TRUE))) %>%
    data.frame() %>%
    tidyr::pivot_longer(all_of(names(method_pretty)), names_to = "method", values_to = "coverage") %>%
    mutate(group = paste0(method, "-", ns[i]), ss = as.character(ns[i]))
})) %>%
  ggplot(aes(x = method_pretty[method], y = coverage, group = group, fill = ss)) +
  geom_boxplot() +
  geom_hline(yintercept = .8) +
  scale_fill_manual(values = colors, name = "Sample Size") +
  ylab("Average Coverage") + xlab(NULL) +
  theme_bw()


## On average provides intervals for about 10 variables

p2 <- do.call(rbind, lapply(1:3, function(i) {
  res <- res_time[[i]]
  colnames(res) <- names(method_pretty)
  res %>%
    data.frame() %>%
    tidyr::pivot_longer(all_of(names(method_pretty)), names_to = "method", values_to = "time") %>%
    mutate(group = paste0(method, "-", ns[i]), ss = as.character(ns[i]))
})) %>%
  ggplot(aes(x = method_pretty[method], y = log10(time), group = group, fill = ss)) +
  geom_boxplot() +
  scale_fill_manual(values = colors, name = "Sample Size") +
  ylab(expression(log10(time))) + xlab(NULL) +
  theme_bw()



p3 <- do.call(rbind, lapply(1:3, function(i) {
  res <- res_width[[i]]
  colnames(res) <- names(method_pretty)
  res %>%
    data.frame() %>%
    tidyr::pivot_longer(all_of(names(method_pretty)), names_to = "method", values_to = "width") %>%
    mutate(group = paste0(method, "-", ns[i]), ss = as.character(ns[i]))
})) %>%
  ggplot(aes(x = method_pretty[method], y = width, group = group, fill = ss)) +
  geom_boxplot() +
  scale_fill_manual(values = colors, name = "Sample Size") +
  ylab(expression(log10(`Median Width`))) + xlab(NULL) +
  theme_bw()

## Get summaries of times failed / infinite width / number of variables selected for si

p4 <- do.call(rbind, lapply(1:3, function(i) {
  res <- res_lambda[[i]]
  colnames(res) <- names(method_pretty)
  res %>%
    data.frame() %>%
    tidyr::pivot_longer(all_of(names(method_pretty)), names_to = "method", values_to = "lambda") %>%
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
          axis.text.x = element_text(angle = 15))
})

glist[[2]] <- glist[[2]] +
  theme(legend.position = c(0.2, 0.2))

bottom_label <- textGrob("Method", gp = gpar(fontsize = 12))

suppressMessages({
  pdf("./fig/lassoboot_comparison_laplace_100_other.pdf", height = 8)
  g <- grid.arrange(grobs = glist, ncol = 2, nrow = 2, bottom = bottom_label)
  dev.off()
  gobj <- grid.arrange(grobs = glist, ncol = 2, nrow = 2, bottom = bottom_label)
  save(gobj, file = glue("{res_dir}/web/rds/lassoboot_comparison_laplace_100_other_{method}.rds"))
})
