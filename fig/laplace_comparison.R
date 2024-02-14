## Setup
source("./fig/setup/setup.R")

## Load Data
## method <- "bucketfill"

plots <- list()

# methods <- c("traditional", "sample", "debiased", "zerosample2")
methods <- c("selective_inference", "zerosample2", "blp")
n_methods <- length(methods)

per_var_data <- list()
for (i in 1:n_methods) {
  load(glue("{res_dir}/rds/laplace_{methods[i]}.rds"))
  per_var_data[[i]] <- per_var
}
per_var_data <- do.call(rbind, per_var_data) %>%
  data.frame()

cutoff <- 3
ns <- unique(per_var_data$n)
for (j in 1:length(ns)) {

  model_res <- per_var_data %>%
    mutate(
      covered = lower <= truth & upper >= truth,
      mag_truth = abs(truth), covered = as.numeric(covered)
    )


  # methods <- unique(model_res$method)
  line_data <- list()
  line_data_avg <- list()
  for (i in 1:length(methods)) {
    print(methods[i])
    tmp <- model_res %>%
      filter(method == methods[i] & n == ns[j])

    if (i == 1) {
      density_data <- density(abs(tmp$truth), bw = .25, n = cutoff * 100 + 1, from = 0, to = cutoff)
      density_data <- data.frame(x = density_data$x, density = density_data$y)
    }

    tmp %>%
      filter(!is.na(estimate)) %>%
      group_by(n) %>%
      summarise(
        perc_succ = length(unique(group))
      ) %>%
      left_join(
        tmp %>%
          group_by(n) %>%
          summarise(
            perc_incl = mean(!is.na(estimate))
          )) %>%
      print()

    tmp <- tmp %>%
      filter(!is.na(estimate))

    fit <- gam(covered ~ s(mag_truth) + s(group, bs = "re"), data = tmp, family = binomial)
    xs <- seq(0, cutoff, by = .01)
    ys <- predict(fit, data.frame(mag_truth = xs, group = 101), type ="response")
    line_data[[i]] <- data.frame(x = xs, y = ys, method = methods[i])

    line_data_avg[[i]] <- data.frame(avg = mean(tmp$covered), method = methods_pretty[methods[i]])

  }

  line_data_avg <- do.call(rbind, line_data_avg)
  line_data <- do.call(rbind, line_data)


  plots[[j]] <- ggplot() +
    geom_line(data = line_data %>% mutate(method = methods_pretty[method]), aes(x = x, y = y, color = method)) +
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
  pdf("./fig/laplace_comparison.pdf", height = 6.5)
  grid.arrange(grobs = list(p1, p2, p3), nrow = 3, ncol = 1)
  dev.off()
  if (save_rds) {
    gobj <- grid.arrange(grobs = list(p1, p2, p3), nrow = 3, ncol = 1)
    save(gobj, file = glue("{res_dir}/web/rds/laplace_comparison.rds"))
  }
})

