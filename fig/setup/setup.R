rm(list=ls())
res_dir <- switch(Sys.info()['user'],
                  'pbreheny' = '~/res/lasso-boot',
                  'loganharris' = '../lasso-boot')

.libPaths(paste0(res_dir, "/local"))
quietlyLoadPackage <- function(package) {
  suppressPackageStartupMessages(library(package, character.only = TRUE))
}

packages <- c("dplyr", "tidyr", "ggplot2", "ncvreg", "gridExtra", "scales", "tidyr", "kableExtra", "grid")

.libPaths(paste0(res_dir, "/local"))
lapply(packages, quietlyLoadPackage)
colors <- palette()[c(2, 4, 3, 6)]
sec_colors <- c("black", "grey62")
background_colors <- c("#E2E2E2", "#F5F5F5")
