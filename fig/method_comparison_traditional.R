## Setup
source("./fig/setup/setup.R")

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
  head(30) %>%
  ggplot() +
  geom_errorbar(aes(xmin = lower, xmax = upper, y = variable)) +
  geom_point(aes(x = estimate, y = variable)) +
  theme_bw() +
  labs(x = "Estimate", title = paste0("Trad. Bootstrap - Coverage: ", round(c1 * 100, 1), "%")) +
  ylab(NULL)

## Lasso Boot
p2 <- plot(lasso_boot, n = 30) +
  ggtitle(paste0("Lasso Bootstrap - Coverage: ", round(c2 * 100, 1), "%")) +
  ylab(NULL) +
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  )

left_label <- textGrob("Variable", gp = gpar(fontsize = 10), rot = 90)

suppressMessages({
  pdf("./fig/method_comparison_traditional.pdf", height = 4)
  grid.arrange(p1, p2, ncol = 2, left = left_label)
  dev.off()
  png("./fig/method_comparison_traditional.png", width = 1000, height = 600)
  grid.arrange(p1, p2, ncol = 2)
  dev.off()
})
