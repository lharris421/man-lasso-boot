## Setup
source("./fig/setup/setup.R")

## Load Data
n <- 60
p <- 60
load(glue("{res_dir}/rds/lassoboot_comparison_traditional_n{n}_p{p}.rds"))
# method <- "bucketfill"

## Traditional Bootstrap
plots <- list()
for (i in 1:length(methods)) {
  ci <- ci.boot.ncvreg(res[[i]], method = method)
  cov <- mean(ci$lower <= dat$beta & dat$beta <= ci$upper) ## Traditional
  plots[[i]] <- plot(res[[i]], n = 30, method = method) +
    ggtitle(glue("{method_pretty[methods[i]]} - Coverage: {round(cov * 100, 1)} %")) +
    ylab(NULL) +
    theme(
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank()
    )
}


left_label <- textGrob("Variable", gp = gpar(fontsize = 10), rot = 90)

suppressMessages({
  pdf("./fig/lassoboot_comparison_traditional.pdf", width = 7.5)
  grid.arrange(grobs = plots, nrow = 3, left = left_label)
  dev.off()
  gobj <- grid.arrange(grobs = plots, nrow = 3, left = left_label)
  save(gobj, file = glue("{res_dir}/web/rds/lassoboot_comparison_traditional_{method}.rds"))
})
