## Setup
source("./fig/setup/setup.R")

## Load Data
quantiles <- "disturbed"
method <- "quantile"
load(glue("{res_dir}/rds/method_comparison_laplace_100_{quantiles}_{method}.rds"))

plots <- list()
method_pretty <- c("hdi" = "BLP", "lasso_sample" = "Lasso Bootstrap", "si" = "Selective Inference")
cutoff <- 3

for (j in 1:3) {

  all_coverages_i <- res_coverage[[j]]
  model_res <- do.call(rbind, all_coverages_i) %>%
    data.frame() %>%
    pivot_longer(si:lasso_sample, names_to = "method", values_to = "covered") %>%
    mutate(mag_truth = abs(truth), covered = as.numeric(covered))

  library(lme4)

  methods <- unique(model_res$method)
  line_data <- list()
  for (i in 1:length(methods)) {
    print(paste0(j, "-", "i"))
    tmp <- model_res %>%
      filter(method == methods[i])

    fit <- gam(covered ~ s(mag_truth) + s(group, bs = "re"), data = tmp, family = binomial)
    xs <- seq(0, cutoff, by = .01)
    ys <- predict(fit, data.frame(mag_truth = xs, group = 101), type ="response")
    line_data[[i]] <- data.frame(x = xs, y = ys, method = methods[i])
  }

  line_data <- do.call(rbind, line_data)


  plots[[j]] <- ggplot() +
    geom_line(data = line_data %>% mutate(method = method_pretty[method]), aes(x = x, y = y, color = method)) +
    theme_bw() +
    xlab(expression(abs(beta))) +
    ylab(NULL) +
    annotate("text", x = 0.1, y = 0.1, label = paste0("N = ", ns[j]), size = 5) +
    coord_cartesian(ylim = c(0, 1)) +
    scale_color_manual(name = "Method", values = colors)


}

plots[[1]] <- plots[[1]] +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.background = element_rect(fill = "transparent"))
plots[[2]] <- plots[[2]] +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")
plots[[3]] <- plots[[3]] +
  theme(
    legend.position = "none")


p1 <- plots[[1]]
p2 <- plots[[2]]
p3 <- plots[[3]]

suppressMessages({
  pdf("./fig/method_comparison_laplace_100.pdf", height = 5)
  grid.arrange(grobs = list(p1, p2, p3), nrow = 3, heights = c(150, 150, 180))
  dev.off()
  png("./fig/method_comparison_laplace_100.png", width = 700, height = 500)
  grid.arrange(grobs = list(p1, p2, p3), nrow = 3)
  dev.off()
})
