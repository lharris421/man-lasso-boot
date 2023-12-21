## Setup
source("./fig/setup/setup.R")

## Load Data
load(paste0(res_dir, "/rds/method_comparison_highcorr_100.rds")) # n = 50

plot_ridge <- function(ridge_ci, n = 30, quiet = TRUE) {

  plot_res <- ridge_ci[-1,] %>%
    data.frame()

  plot_res$variable <- rownames(plot_res)
  plot_res <- plot_res %>%
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
  coord_cartesian(xlim = c(-1, 2)) +
  theme(legend.position = "none",
        plot.background = element_rect(fill=background_colors[2])) +
  ylab(NULL) +
  xlab(NULL) +
  scale_color_manual(values = colors)

colnames(ridge_example) <- tolower(colnames(ridge_example))
p2 <- plot_ridge(ridge_example) +
  coord_cartesian(xlim = c(-1, 2)) +
  ylab(NULL) +
  xlab(NULL) +
  theme(plot.background = element_rect(fill=background_colors[2]))

## Lasso
p3 <- do.call(rbind, lasso_cis_s) %>%
  data.frame() %>%
  filter(variable %in% c("A1", "B1", "N1")) %>%
  mutate(group = rep(1:100, each = 3), group = paste0(variable, group)) %>%
  pivot_longer(lower:upper, names_to = "bound", values_to = "value") %>%
  ggplot() +
  geom_boxplot(aes(x = value, y = variable, color = bound)) +
  theme_bw() +
  coord_cartesian(xlim = c(-1, 2)) +
  theme(legend.position = "none", plot.background = element_rect(fill=background_colors[1])) +
  ylab(NULL) +
  xlab(NULL) +
  scale_color_manual(values = colors)

p4 <- plot(lasso_example_s) +
  coord_cartesian(xlim = c(-1, 2)) +
  ylab(NULL) +
  xlab(NULL) +
  theme(plot.background = element_rect(fill=background_colors[1]))

left_label <- textGrob("  Lasso                                            Ridge", gp = gpar(fontsize = 12), rot = 90)
suppressMessages({
  pdf("./fig/method_comparison_highcorr_100.pdf", height = 5)
  grid.arrange(grobs = list(p1, p2, p3, p4), ncol = 2, left = left_label)
  dev.off()
  png("./fig/method_comparison_highcorr_100.png", width = 1000, height = 800)
  grid.arrange(grobs = list(p1, p2, p3, p4), ncol = 2)
  dev.off()
})

