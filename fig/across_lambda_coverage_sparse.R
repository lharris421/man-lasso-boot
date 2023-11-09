#sink("/dev/null")
res_dir <- switch(Sys.info()['user'],
                     'pbreheny' = '~/res/lasso-boot')

quietlyLoadPackage <- function(package) {
  suppressPackageStartupMessages(library(package, character.only = TRUE))
}

packages <- c("dplyr", "ggplot2", "scales", "gridExtra")

lapply(packages, quietlyLoadPackage)

## Load Data
load(paste0(res_dir, "/rds/across_lambda_coverage_sparse.rds"))

# Create a new transformation for reversed log10
log10_trans <- function() {
  trans_new(name = 'rev_log10',
            transform = function(x) -log10(x),
            inverse = function(x) 10^(-x))
}


make_plot <- function(plot_res) {

  plot_data <- plot_res$plot_data
  lambda_min <- plot_res$lambda_min
  n <- plot_res$n

  coverages <- plot_data %>%
    mutate(covered = truth >= lower & truth <= upper) %>%
    group_by(lambda) %>%
    summarise(coverage = mean(covered)) %>%
    ungroup()

  coverages_each <- plot_data %>%
    mutate(covered = truth >= lower & truth <= upper) %>%
    group_by(lambda, mag = abs(truth)) %>%
    summarise(coverage = mean(covered)) %>%
    ungroup()

  # Create a scaling factor based on the range of the primary axis
  scaling_factor <- max(plot_data$width)

  # Scale the 'coverage' data for plotting with 'geom_smooth()'
  coverages$scaled_coverage <- coverages$coverage * (scaling_factor / 1)
  coverages_each$scaled_coverage <- coverages_each$coverage * (scaling_factor / 1)

  print(tail(coverages_each))

  ## "Selected via CV" = "blue", "Truth" = "red"
  ggplot() +
    scale_x_continuous(trans = log10_trans(),
                       breaks = trans_breaks('log10', function(x) 10^x),
                       labels = trans_format('log10', math_format(10^.x))) +
    geom_vline(xintercept = lambda_min, linetype = "dashed", color = "blue", linewidth = .5) +
    # geom_smooth(data = coverages, aes(x = lambda, y = scaled_coverage), se = FALSE, color = "grey") +
    geom_line(data = coverages, aes(x = lambda, y = scaled_coverage), color = "grey") +
    geom_line(data = coverages_each, aes(x = lambda, y = scaled_coverage, group = mag, color = as.character(mag))) +
    # geom_smooth(data = coverages_each, aes(x = lambda, y = scaled_coverage, group = mag, color = as.character(mag)), se = FALSE, method = "glm", method.args=list(family=binomial)) +
    geom_jitter(data = plot_data, aes(x = lambda, y = width, color = as.character(abs(truth))), alpha = .7, width = .05) +
    xlab(expression(lambda)) + ylab("Interval Width") +
    theme_bw() +
    # scale_color_gradient(name = expression(abs(beta)), low = "lightgrey", high = "black") +
    scale_color_discrete(name = expression(abs(beta))) +
    scale_y_continuous(
      name = "Interval Width",
      sec.axis = sec_axis(~ ., name = "Overall Coverage",
                          breaks = seq(0, scaling_factor, by = scaling_factor/10),
                          labels = seq(0, 1, by = .10))
    ) +
    theme(axis.line.y.right = element_line(color = "darkgrey"),
          axis.ticks.y.right = element_line(color = "darkgrey")) +
    ggtitle(paste0("N = ", n))

}

suppressMessages({
plots <- lapply(plot_res, make_plot)

pdf("./fig/tmp/across_lambda_coverage_sparse.pdf", width = 10, height = 10)
  grid.arrange(grobs = plots, ncol = 1)
})

dev.off()

#sink()
