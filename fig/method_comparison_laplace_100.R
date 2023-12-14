rm(list=ls())
res_dir <- switch(Sys.info()['user'],
                  'pbreheny' = '~/res/lasso-boot',
                  'loganharris' = '../lasso-boot')

quietlyLoadPackage <- function(package) {
  suppressPackageStartupMessages(library(package, character.only = TRUE))
}

packages <- c("dplyr", "ggplot2", "ncvreg", "gridExtra", "scales", "tidyr", "kableExtra")

.libPaths(paste0(res_dir, "/local"))
lapply(packages, quietlyLoadPackage)

## Load Data
load(paste0(res_dir, "/rds/method_comparison_laplace_100.rds"))


ns <- c(20, 30, 60)

plots <- list()
for (j in 1:3) {

  all_coverages_i <- all_coverages[[j]]
  model_res <- do.call(rbind, all_coverages_i) %>%
    data.frame() %>%
    pivot_longer(si:lasso_sample, names_to = "method", values_to = "covered") %>%
    mutate(mag_truth = abs(truth), covered = as.numeric(covered))

  library(lme4)

  methods <- unique(model_res$method)
  line_data <- list()
  for (i in 1:length(methods)) {
    tmp <- model_res %>%
      filter(method == methods[i])

    fit <- lme4::glmer(covered ~ mag_truth + (1|group), data = tmp, family = binomial)
    xs <- seq(0, 4, by = .1)
    ys <- predict(fit, data.frame(mag_truth = xs, group = 101), type ="response", allow.new.levels = TRUE)
    line_data[[i]] <- data.frame(x = xs, y = ys, method = methods[i])
  }

  line_data <- do.call(rbind, line_data)


  plots[[j]] <- ggplot() +
    geom_line(data = line_data, aes(x = x, y = y, color = method)) +
    theme_bw() +
    xlab(expression(abs(beta))) +
    ylab("Coverage") +
    ggtitle(paste0("N: ", ns[j])) +
    coord_cartesian(ylim = c(0, 1))


}

rownames(res_width) <- rownames(res_lambda) <- rownames(res_coverage) <- rownames(res_time) <- c(20, 30, 60)
colnames(res_width) <- colnames(res_lambda) <- colnames(res_coverage) <- colnames(res_time) <- c("Original", "Combined", "Sample")

t1 <- knitr::kable(res_coverage, caption = "Average Coverage [mean (sd)]", booktabs = T) %>%
                  kable_styling(full_width = T)
t2 <- tableGrob(knitr::kable(res_time, caption = "Average Runtime (seconds) [mean (sd)]"))
t3 <- tableGrob(knitr::kable(res_width, caption = "Average Widths [mean (sd)]"))
t4 <- tableGrob(knitr::kable(res_lambda, caption = "Average Lambdas [mean (sd)]"))
p1 <- plots[[1]]
p2 <- plots[[2]]
p3 <- plots[[3]]

suppressMessages({
  pdf("./fig/method_comparison_laplace_100.pdf", width = 7, height = 7)
  grid.arrange(grobs = list(p1, p2, p3), nrow = 3)
  dev.off()
  png("./fig/method_comparison_laplace_100.png", width = 700, height = 700)
  grid.arrange(grobs = list(p1, p2, p3), nrow = 3)
  dev.off()
})

