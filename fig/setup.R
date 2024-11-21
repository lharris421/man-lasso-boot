rm(list=ls())

res_dir <- switch(Sys.info()['user'],
                  'pbreheny' = '~/res/lasso-boot',
                  'loganharris' = '../lasso-boot')

devtools::load_all(res_dir)
rds_path <- glue::glue("{res_dir}/rds/")

packages <- c(
  "dplyr", "tidyr", "ggplot2", "gridExtra", "scales", "kableExtra",
  "grid", "glue", "lme4", "mgcv", "splines", "digest", "indexr",
  "patchwork", "knitr"
)
quietlyLoadPackage <- function(package) {
  suppressPackageStartupMessages(library(package, character.only = TRUE))
}
lapply(packages, quietlyLoadPackage)

colors <- palette()[c(2, 4, 3, 6, 7, 5)]
sec_colors <- c("black", "grey62")
background_colors <- c("#E2E2E2", "#F5F5F5")
#"black"   "#DF536B" "#61D04F" "#2297E6" "#28E2E5" "#CD0BBC" "#F5C710" "gray62"

methods_pretty <- c(
  "traditional" = "Traditional",
  "posterior" = "Posterior",
  "hybrid" = "Hybrid",
  "selective_inference" = "Selective Inference",
  "selectiveinference" = "SI",
  "blp" = "BLP",
  "fullconditional" = "Full Conditional",
  "truncatedzs2" = "Truncated Zero Sample",
  "zerosample2la" = "Lambda Adjusted",
  "debiased_normalized" = "Norm Debias",
  "debiased_corrected" = "Corrected Debias",
  "full_debias" = "Full Debias",
  "MCP" = "MCP", "SCAD" = "SCAD",
  "lasso" = "Hybrid",
  "enet" = "Elastic Net",
  "enet1" = "Elastic Net (0.8)",
  "elastic_net" = "Elastic Net (0.8)",
  "ridge" = "Ridge",
  "lasso_boot" = "Hybrid",
  "lasso_posterior_pipe" = "Posterior (PIPE)",
  "lasso_proj_boot" = "BLP",
  "lasso_proj_boot_shortcut" = "BLP",
  "mcp_boot" = "MCP Hybrid Bootstrap",
  "mcp" = "MCP Posterior"
)
methods <- list(
  "lasso_boot" = list(method = "boot_ncv", method_arguments = list(penalty = "lasso", submethod = "hybrid")),
  "mcp_boot" = list(method = "boot_ncv", method_arguments = list(penalty = "MCP", submethod = "hybrid")),
  "traditional" = list(method = "boot_ncv", method_arguments = list(penalty = "lasso", submethod = "traditional")),
  "posterior" = list(method = "boot_ncv", method_arguments = list(penalty = "lasso", submethod = "posterior")),
  "lasso_posterior_pipe" = list(method = "posterior", method_arguments = list(penalty = "lasso")),
  "selective_inference" = list(method = "selective_inference", method_arguments = list()),
  "lasso_proj_boot" = list(method = "blp", method_arguments = list()),
  "lasso_proj_boot_shortcut" = list(method = "blp", method_arguments = list(boot.shortcut = TRUE)),
  "elastic_net" = list(method = "boot_ncv", method_arguments = list(penalty = "lasso", submethod = "hybrid", enet_alpha = 0.8)),
  "ridge" = list(method = "ridge", method_arguments = list()),
  "mcp"   = list(method = "posterior", method_arguments = list(penalty = "MCP"))
)
for (i in 1:length(methods)) {
  methods[[i]]$method_arguments["alpha"] <- 0.2
}


