## Setup
source("./fig/setup/setup.R")

## Load Data
method <- "lasso"
n_values <- 100
data_type <- "sparse"
SNR <- 1
alpha <- .2
p <- 100
enet_alpha <- 1
gamma <- NA
modifier <- NA

params_grid <- expand.grid(list(data = data_type, n = n_values, snr = SNR, lambda = "cv",
                                method = method, modifier = modifier,
                                nominal_coverage = (1-alpha) * 100, p = p,
                                alpha = enet_alpha, gamma = gamma))

# Fetching and combining data
coverages <- numeric()
for (i in 1:nrow(params_grid)) {
  res_list <- read_objects(rds_path, params_grid[i,], save_method = "rds")
  res_ci <- res_list$confidence_interval
  res <- res_list$example
}
coverages <- do.call(rbind, res_ci) %>%
  data.frame() %>%
  mutate(covered = lower <= truth & upper >= truth) %>%
  group_by(submethod) %>%
  summarise(coverage = mean(covered))

do.call(rbind, res_ci) %>%
  data.frame() %>%
  mutate(covered = lower <= truth & upper >= truth, mag_truth = abs(truth)) %>%
  group_by(submethod, mag_truth) %>%
  summarise(coverage = mean(covered))

## Traditional Bootstrap
submethods <- unique(res_ci[[1]]$submethod)
plots <- list()
for (i in 1:length(submethods)) {

  true_vals <- res_ci[[1]] %>% filter(submethod == submethods[i]) %>% pull(truth)
  names(true_vals) <- colnames(res$fc_draws)
  ordering <- names(sort(true_vals))[1:30]
  true_vals <- true_vals[1:30]



  cov <- coverages %>%
    filter(submethod == submethods[i]) %>%
    pull(coverage)
  plots[[i]] <- plot(res, method = submethods[i], n = 30) +
    ggtitle(glue("{methods_pretty[submethods[i]]} - Coverage: {round(cov * 100, 1)} %")) +
    ylab("Variable") +
    geom_point(data = data.frame(y = names(true_vals), x = true_vals), aes(x = x, y = y), color = "red", shape = 1, size = 2) +
    theme(
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank()
    )
}


left_label <- textGrob("Variable", gp = gpar(fontsize = 10), rot = 90)

pdf(glue("./fig/ec_lasso_hybrid.pdf"), height = 4, width = 4)
plots[[which(submethods == "hybrid")]]
dev.off()
pdf(glue("./fig/ec_lasso_traditional.pdf"), height = 4, width = 4)
plots[[which(submethods == "traditional")]]
dev.off()
pdf(glue("./fig/ec_lasso_debiased.pdf"), height = 4, width = 4)
plots[[which(submethods == "debiased")]]
dev.off()
pdf(glue("./fig/ec_lasso_posterior.pdf"), height = 4, width = 4)
plots[[which(submethods == "posterior")]]
dev.off()

