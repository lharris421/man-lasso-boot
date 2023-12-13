# rm(list=ls())
res_dir <- switch(Sys.info()['user'],
                     'pbreheny' = '~/res/lasso-boot',
                      'loganharris' = '../lasso-boot')

quietlyLoadPackage <- function(package) {
  suppressPackageStartupMessages(library(package, character.only = TRUE))
}

packages <- c("dplyr", "ggplot2", "ncvreg", "gridExtra")

.libPaths(paste0(res_dir, "/local"))
lapply(packages, quietlyLoadPackage)

## Load Data
load(paste0(res_dir, "/rds/method_comparison_traditional.rds"))

## Coverage
c1 <- mean(trad_res$lower <= dat$beta & dat$beta <= trad_res$upper) ## Traditional
ci <- ci.boot.ncvreg.r(lasso_boot) ## Lasso Boot
c2 <- mean(ci$lower <= dat$beta & dat$beta <= ci$upper)

## Traditional Bootstrap
trad_res <- trad_res %>%
  dplyr::arrange(desc(abs(estimate)))

trad_res$variable <- factor(trad_res$variable, levels = rev(trad_res$variable))

p1 <- trad_res %>%
  ggplot() +
  geom_errorbar(aes(xmin = lower, xmax = upper, y = variable)) +
  geom_point(aes(x = estimate, y = variable)) +
  theme_bw() +
  labs(y = "Variable", x = "Estimate", title = paste0("Traditional Bootstrap - Coverage: ", round(c1 * 100, 1), "%"))

## Lasso Boot
p2 <- plot(lasso_boot, n = 60) +
  ggtitle(paste0("Lasso Bootstrap - Coverage: ", round(c2 * 100, 1), "%"))

suppressMessages({
  plots <- lapply(plot_res, make_plot)
  pdf("./fig/method_comparison_traditional.pdf", width = 10, height = 6)
  grid.arrange(p1, p2, ncol = 2)
  dev.off()
  png("./fig/method_comparison_traditional.png", width = 1000, height = 600)
  grid.arrange(p1, p2, ncol = 2)
  dev.off()
})
