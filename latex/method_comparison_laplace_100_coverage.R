res_dir <- switch(Sys.info()['user'],
                  'pbreheny' = '~/res/lasso-boot',
                  'loganharris' = '../lasso-boot')

load(paste0(res_dir, "/rds/method_comparison_laplace_100.rds"))
rownames(res_width) <- rownames(res_lambda) <- rownames(res_coverage) <- rownames(res_time) <- c(20, 30, 60)
colnames(res_width) <- colnames(res_lambda) <- colnames(res_coverage) <- colnames(res_time) <- c("Selective Inference", "BLP", "Lasso Bootstrap")

library(kableExtra)

kable(res_coverage, format = "latex", booktabs = TRUE, caption = "Average Coverage [mean (sd)]") %>%
  save_kable("./latex/method_comparison_laplace_100_coverage.tex")
