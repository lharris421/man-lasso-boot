res_dir <- switch(Sys.info()['user'],
                  'pbreheny' = '~/res/lasso-boot',
                  'loganharris' = '../lasso-boot')

quietlyLoadPackage <- function(package) {
  suppressPackageStartupMessages(library(package, character.only = TRUE))
}

packages <- c("dplyr", "ggplot2", "ncvreg", "gridExtra", "scales")

.libPaths(paste0(res_dir, "/local"))
lapply(packages, quietlyLoadPackage)

## Load Data
load(paste0(res_dir, "/rds/across_lambda_coverage_laplace.rds"))

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
    geom_vline(xintercept = true_lambda, linetype = "dashed", color = "red", linewidth = .5) +
    geom_vline(xintercept = lambda_min, linetype = "dashed", color = "blue", linewidth = .5) +
    geom_smooth(data = coverages, aes(x = lambda, y = scaled_coverage), se = FALSE, color = "grey") +
    geom_jitter(data = plot_data, aes(x = lambda, y = width, color = abs(truth)), alpha = .7, width = .05) +
    xlab(expression(lambda)) + ylab("Interval Width") +
    theme_bw() +
    # scale_color_gradient(name = expression(abs(beta)), low = "lightgrey", high = "black") +
    scale_color_continuous(name = expression(abs(beta))) +
    scale_y_continuous(
      name = "Interval Width",
      sec.axis = sec_axis(~ ., name = "Overall Coverage",
                          breaks = seq(0, scaling_factor, by = scaling_factor/10),
                          labels = seq(0, 1, by = .10)),
      labels=scaleFUN
    ) +
    theme(axis.line.y.right = element_line(color = "darkgrey"),
          axis.ticks.y.right = element_line(color = "darkgrey")) +
    ggtitle(paste0("N = ", n))  +
    coord_cartesian(xlim = c(10^(.45), 10^(-2.65)))
  ## Adjuist y axis rounding

}

plots <- lapply(plot_res, make_plot)

pdf("./fig/across_lambda_coverage_laplace.pdf", width = 10, height = 11)
suppressMessages({
  grid.arrange(grobs = plots, ncol = 1)
})

dev.off()
