## Setup
source("./fig/setup/setup.R")

## Load Data
methods <- c("traditional", "zerosample2")

## Coverages first
res <- list()
for (i in 1:length(methods)) {
  load(glue("{res_dir}/rds/epsilon_conundrum_{methods[i]}.rds"))
  res[[i]] <- confidence_interval
}
names(res) <- methods

coverages <- numeric()
for (i in 1:length(methods)) {
  coverages[i] <- do.call(rbind, res[[methods[i]]]) %>%
    data.frame() %>%
    mutate(covered = lower <= truth & upper >= truth) %>%
    pull(covered) %>%
    mean()
}
names(coverages) <- methods

res <- list()
for (i in 1:length(methods)) {
  load(glue("{res_dir}/rds/epsilon_conundrum_example_{methods[i]}.rds"))
  res[[i]] <- example
}
names(res) <- methods

## Traditional Bootstrap
plots <- list()
for (i in 1:length(methods)) {
  ci <- ci.boot.ncvreg(res[[i]], method = method)
  # cov <- mean(ci$lower <= dat$beta & dat$beta <= ci$upper) ## Traditional
  cov <- coverages[methods[i]]
  plots[[i]] <- plot(res[[i]], n = 30, method = method) +
    ggtitle(glue("{methods_pretty[methods[i]]} - Coverage: {round(cov * 100, 1)} %")) +
    ylab(NULL) +
    theme(
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank()
    )
}


left_label <- textGrob("Variable", gp = gpar(fontsize = 10), rot = 90)

suppressMessages({
  pdf("./fig/epsilon_conundrum.pdf", height = 4 * ceiling(length(methods) / 2))
  grid.arrange(grobs = plots, nrow = ceiling(length(methods) / 2), left = left_label)
  dev.off()
  if (save_rds) {
    gobj <- grid.arrange(grobs = plots, nrow = ceiling(length(methods) / 2), left = left_label)
    save(gobj, file = glue("{res_dir}/web/rds/epsilon_conundrum.rds"))
  }
})
