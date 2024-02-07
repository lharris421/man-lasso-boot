rm(list=ls())
unloadNamespace("ncvreg")
res_dir <- switch(Sys.info()['user'],
                  'pbreheny' = '~/res/lasso-boot',
                  'loganharris' = '../lasso-boot')

.libPaths(paste0(res_dir, "/local"))
quietlyLoadPackage <- function(package) {
  suppressPackageStartupMessages(library(package, character.only = TRUE))
}

packages <- c(
  "dplyr", "tidyr", "ggplot2", "ncvreg", "gridExtra", "scales", "kableExtra",
  "grid", "glue", "lme4", "gam", "mgcv", "splines"
)

.libPaths(paste0(res_dir, "/local"))
lapply(packages, quietlyLoadPackage)
colors <- palette()[c(2, 4, 3, 6, 7, 5)]
sec_colors <- c("black", "grey62")
background_colors <- c("#E2E2E2", "#F5F5F5")

method <- "quantile"
methods_pretty <- c(
  "traditional" = "Traditional",
  "sample" = "Random Sample",
  "debiased" = "Debias",
  "acceptreject" = "Accept/Reject",
  "zerosample1" = "Zero Sample Single",
  "zerosample2" = "Zero Sample",
  "selective_inference" = "Selective Inference",
  "blp" = "Bootstrap Lasso Projection"
)

save_rds <- FALSE
