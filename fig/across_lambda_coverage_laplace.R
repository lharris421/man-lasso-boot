## Setup
source("./fig/setup/setup.R")

## Load Data
quantiles <- "zerosample"
method <- "quantile"
load(glue("{res_dir}/rds/across_lambda_coverage_laplace_{quantiles}_{method}.rds"))

# Create a new transformation for reversed log10
log10_trans <- function() {
  trans_new(name = 'rev_log10',
            transform = function(x) -log10(x),
            inverse = function(x) 10^(-x))
}

scaleFUN <- function(x) sprintf("%.1f", x)

make_plot <- function(plot_res) {

  plot_data <- plot_res$plot_data
  true_lambda <- plot_res$true_lambda
  lambda_min <- plot_res$lambda_min
  n <- plot_res$n

  coverages <- plot_data %>%
    mutate(covered = truth >= lower & truth <= upper) %>%
    group_by(lambda) %>%
    summarise(coverage = mean(covered))

  # Create a scaling factor based on the range of the primary axis
  scaling_factor <- max(plot_data$width)

  # Scale the 'coverage' data for plotting with 'geom_smooth()'
  coverages$scaled_coverage <- coverages$coverage * (scaling_factor / 1)

  ## "Selected via CV" = "blue", "Truth" = "red"
  ggplot() +
    scale_x_continuous(trans = log10_trans(),
                       breaks = trans_breaks('log10', function(x) 10^x),
                       labels = trans_format('log10', math_format(10^.x))) +
    geom_vline(xintercept = true_lambda, linetype = "dashed", color = sec_colors[1], linewidth = .5) +
    geom_vline(xintercept = lambda_min, linetype = "dashed", color = sec_colors[2], linewidth = .5) +
    geom_smooth(data = coverages, aes(x = lambda, y = scaled_coverage), se = FALSE, color = "grey", linewidth = .75) +
    geom_jitter(data = plot_data, aes(x = lambda, y = width, color = abs(truth)), alpha = .7, width = .05, size = 0.75) +
    theme_bw() +
    # scale_color_continuous(name = expression(abs(beta))) +
    scale_color_gradient(name = expression(abs(beta)), low = colors[2], high = colors[1]) +
    scale_y_continuous(
      name = NULL,
      sec.axis = sec_axis(~ ., name = NULL,
                          breaks = seq(0, scaling_factor, by = scaling_factor/10),
                          labels = seq(0, 1, by = .10)),
      labels=scaleFUN
    ) +
    theme(axis.line.y.right = element_line(color = "grey"),
          axis.ticks.y.right = element_line(color = "grey"),
          axis.text.y = element_text(size = 9),
          axis.text.y.right = element_text(size = 9),
          legend.position = c(0.875, 0.15),
          legend.direction = "horizontal",
          legend.background = element_rect(fill = NA),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.key.width = unit(0.6, "cm"),
          legend.key.height = unit(0.3, "cm")) +
    coord_cartesian(xlim = c(10^(.45), 10^(-2.65))) +
    annotate("text", x = 10^.4, y = max(plot_data$width)*.98, label = paste0("N = ", n), size = 3.5)
  ## Adjuist y axis rounding

}

plots <- lapply(plot_res, make_plot)
#plots <- plots[c(1, 2, 4)]
plots[[1]] <- plots[[1]] +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
  )
plots[[2]] <- plots[[2]] +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
plots[[3]] <- plots[[3]] +
  xlab(expression(lambda))

library(grid)
left_label <- textGrob("Interval Widths", gp = gpar(fontsize = 12), rot = 90)
right_label <- textGrob("Coverage", gp = gpar(fontsize = 12), rot = 270)

suppressMessages({
  pdf("./fig/across_lambda_coverage_laplace.pdf", height = 5)
  g <- grid.arrange(grobs = plots, ncol = 1, heights = c(150, 150, 200), left = left_label, right = right_label)
  dev.off()
  png("./fig/across_lambda_coverage_laplace.png", width = 500, height = 600)
  grid.arrange(grobs = plots, ncol = 1)
  dev.off()
})
