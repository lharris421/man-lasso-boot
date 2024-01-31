## Setup
source("./fig/setup/setup.R")

## Load Data
method <- "bucketfill"
load(glue("{res_dir}/rds/lassoboot_comparison_laplace_100_{method}.rds"))

plots <- list()

n_methods <- length(method_pretty)

library(mgcv)
library(splines)

cutoff <- 3
for (j in 1:3) {

  all_coverages_i <- res_coverage[[j]]
  model_res <- do.call(rbind, all_coverages_i) %>%
    data.frame() %>%
    pivot_longer(all_of(names(method_pretty)), names_to = "method", values_to = "covered") %>%
    mutate(mag_truth = abs(truth), covered = as.numeric(covered))

  methods <- unique(model_res$method)
  line_data <- list()
  line_data_avg <- list()
  for (i in 1:length(methods)) {
    tmp <- model_res %>%
      filter(method == methods[i])

    # fit <- lme4::glmer(covered ~ mag_truth + mag_truth^2 + (1|group), data = tmp, family = binomial, control = glmerControl(optimizer = "bobyqa"))
    fit <- gam(covered ~ s(mag_truth) + s(group, bs = "re"), data = tmp, family = binomial)
    # fit <- gam(covered ~ s(mag_truth), data = tmp, family = binomial)
    xs <- seq(0, cutoff, by = .01)
    # ys <- predict(fit, data.frame(mag_truth = xs, group = 101), type ="response", allow.new.levels = TRUE)
    ys <- predict(fit, data.frame(mag_truth = xs, group = 101), type ="response")
    # ys <- predict(fit, data.frame(mag_truth = xs), type ="response")
    line_data[[i]] <- data.frame(x = xs, y = ys, method = methods[i])

    if (i == 1) {
      density_data <- density(abs(tmp$truth), bw = .25, n = cutoff * 100 + 1, from = 0, to = cutoff)
      density_data <- data.frame(x = density_data$x, density = density_data$y)
    }

    line_data_avg[[i]] <- data.frame(avg = sum(ys * density_data$density) / sum(density_data$density), method = method_pretty[methods[i]])

  }

  line_data_avg <- do.call(rbind, line_data_avg)
  line_data <- do.call(rbind, line_data)


  plots[[j]] <- ggplot() +
    geom_line(data = line_data %>% mutate(method = method_pretty[method]), aes(x = x, y = y, color = method)) +
    geom_hline(data = line_data_avg, aes(yintercept = avg, color = method), linetype = 2) +
    geom_hline(aes(yintercept = .8), linetype = 1) +
    geom_area(data = density_data, aes(x = x, y = density / max(density)), fill = "grey", alpha = 0.5) +
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
        legend.position = c(.95, .05),
        legend.justification = c("right", "bottom"),
        legend.direction = "horizontal",
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
  pdf("./fig/lassoboot_comparison_laplace_100.pdf", height = 6.5)
  grid.arrange(grobs = list(p1, p2, p3), nrow = 3, ncol = 1)
  dev.off()
  gobj <- grid.arrange(grobs = list(p1, p2, p3), nrow = 3, ncol = 1)
  save(gobj, file = glue("{res_dir}/web/rds/lassoboot_comparison_laplace_100_{method}.rds"))
})

