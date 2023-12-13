res_dir <- switch(Sys.info()['user'],
                  'pbreheny' = '~/res/lasso-boot',
                  'loganharris' = '/Users/loganharris/github/lasso-boot')

unloadNamespace("ncvreg")
.libPaths(paste0(res_dir, "/local"))
quietlyLoadPackage <- function(package) {
  suppressPackageStartupMessages(library(package, character.only = TRUE))
}

packages <- c("ncvreg", "dplyr", "tidyr", "ggplot2", "gridExtra", "scales")
lapply(packages, quietlyLoadPackage)

## Load Data
load(paste0(res_dir, "/rds/lassoboot_method_comparison_highcorr_100_n30.rds"))

plot_ridge <- function(ridge_ci, n = 30, quiet = TRUE) {

  plot_res <- ridge_ci %>%
    data.frame()

  plot_res$variable <- rownames(plot_res)
  plot_res %>%
    dplyr::arrange(desc(abs(estimate))) %>%
    head(n)

  plot_res$variable <- factor(plot_res$variable, levels = rev(plot_res$variable))

  plot_res %>%
    ggplot() +
    geom_errorbar(aes(xmin = lower, xmax = upper, y = variable)) +
    geom_point(aes(x = estimate, y = variable)) +
    theme_bw() +
    labs(y = "Variable", x = "Estimate")

}

## Ridge
ridge_res <- do.call(rbind, ridge_cis)
variables <- rownames(ridge_res)
p1 <- ridge_res %>%
  data.frame() %>%
  mutate(variable = variables) %>%
  filter(variable %in% c("A1", "B1", "N1")) %>%
  pivot_longer(Lower:Upper, names_to = "bound", values_to = "value") %>%
  ggplot() +
  geom_boxplot(aes(x = value, y = variable, color = bound)) +
  theme_bw() +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red", linewidth = .5) +
  geom_vline(xintercept = 1.0, linetype = "dashed", color = "blue", linewidth = .5) +
  coord_cartesian(xlim = c(-2, 2)) +
  ylab("Ridge")

colnames(ridge_example) <- tolower(colnames(ridge_example))
p2 <- plot_ridge(ridge_example[-1,]) +
  coord_cartesian(xlim = c(-0.2, 1.2))

## Bayes
# bayes_res <- do.call(rbind, bayes_cis)
# variables <- rownames(bayes_res)
# p9 <- bayes_res %>%
#   data.frame() %>%
#   filter(variable %in% c("A1", "B1", "N1")) %>%
#   pivot_longer(lower:upper, names_to = "bound", values_to = "value") %>%
#   ggplot() +
#   geom_boxplot(aes(x = value, y = variable, color = bound)) +
#   theme_bw() +
#   geom_vline(xintercept = 0.5, linetype = "dashed", color = "red", linewidth = .5) +
#   geom_vline(xintercept = 1.0, linetype = "dashed", color = "blue", linewidth = .5) +
#   coord_cartesian(xlim = c(-2, 2)) +
#   ylab("Bayes")
#
# colnames(bayes_example) <- tolower(colnames(bayes_example))
# rownames(bayes_example) <- rownames(ridge_example[-1,])
# p10 <- plot_ridge(bayes_example) +
#   coord_cartesian(xlim = c(-0.2, 1))

## Lasso
p3 <- do.call(rbind, lasso_cis_o) %>%
  data.frame() %>%
  filter(variable %in% c("A1", "B1", "N1")) %>%
  mutate(group = rep(1:100, each = 3), group = paste0(variable, group)) %>%
  pivot_longer(lower:upper, names_to = "bound", values_to = "value") %>%
  ggplot() +
  geom_boxplot(aes(x = value, y = variable, color = bound)) +
  # geom_line(aes(x = value, y = variable, group = group, color = variable), position = position_nudge(y = .2)) +
  theme_bw() +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red", linewidth = .5) +
  geom_vline(xintercept = 1.0, linetype = "dashed", color = "blue", linewidth = .5)+
  coord_cartesian(xlim = c(-2, 2)) +
  ylab("Lasso Original")

p4 <- plot(lasso_example_o) +
  coord_cartesian(xlim = c(-0.2, 1.2))

## Lasso Combined
p5 <- do.call(rbind, lasso_cis_c) %>%
  data.frame() %>%
  filter(variable %in% c("A1", "B1", "N1")) %>%
  mutate(group = rep(1:100, each = 3), group = paste0(variable, group)) %>%
  pivot_longer(lower:upper, names_to = "bound", values_to = "value") %>%
  ggplot() +
  geom_boxplot(aes(x = value, y = variable, color = bound)) +
  # geom_line(aes(x = value, y = variable, group = group, color = variable), position = position_nudge(y = .2)) +
  theme_bw() +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red", linewidth = .5) +
  geom_vline(xintercept = 1.0, linetype = "dashed", color = "blue", linewidth = .5)+
  coord_cartesian(xlim = c(-2, 2)) +
  ylab("Lasso Combine")

p6 <- ncvreg:::plot.boot.ncvreg.r(lasso_example_c) +
  coord_cartesian(xlim = c(-0.2, 1.2))

## Lasso sample
p7 <- do.call(rbind, lasso_cis_s) %>%
  data.frame() %>%
  filter(variable %in% c("A1", "B1", "N1")) %>%
  mutate(group = rep(1:100, each = 3), group = paste0(variable, group)) %>%
  pivot_longer(lower:upper, names_to = "bound", values_to = "value") %>%
  ggplot() +
  geom_boxplot(aes(x = value, y = variable, color = bound)) +
  # geom_line(aes(x = value, y = variable, group = group, color = variable), position = position_nudge(y = .2)) +
  theme_bw() +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red", linewidth = .5) +
  geom_vline(xintercept = 1.0, linetype = "dashed", color = "blue", linewidth = .5)+
  coord_cartesian(xlim = c(-2, 2)) +
  ylab("Lasso Sample")

p8 <- ncvreg:::plot.boot.ncvreg.r(lasso_example_s) +
  coord_cartesian(xlim = c(-0.2, 1.2))

## Plotting
png("./fig/lassoboot_method_comparison_highcorr_100_n30.png", width = 1000, height = 2000)
suppressMessages({
  grid.arrange(grobs = list(p1, p2, p3, p4, p5, p6, p7, p8), ncol = 2)
})

dev.off()
