## Setup
source("./fig/setup/setup.R")

## Load Data
alpha <- .2
base_params <- list(data = "various",
                    snr = 1,
                    n = 100,
                    p = 100,
                    correlation_structure = "exchangeable",
                    correlation = 0,
                    correlation_noise = NA,
                    method = "zerosample2",
                    ci_method = "quantile",
                    lambda = "across",
                    nominal_coverage = alpha * 100)

read_objects(rds_path, expand.grid(base_params))

dist_types <- c("Sparse 1", "Sparse 2", "Sparse 3", "Normal", "Laplace", "T")
all_res[[1]]$dist_type <- dist_types[1]
all_res[[2]]$dist_type <- dist_types[2]
all_res[[3]]$dist_type <- dist_types[3]
all_res[[4]]$dist_type <- dist_types[4]
all_res[[5]]$dist_type <- dist_types[5]
all_res[[6]]$dist_type <- dist_types[6]

table(all_res[[1]]$group)
table(all_res[[1]]$truth)

lambda_range <- unlist(all_lambdas)
xmax <- max(lambda_range); xmin <- min(lambda_range)

plots <- lapply(all_res, plot_function)
plots <- list(plots[[1]], plots[[2]], plots[[3]], plots[[5]], plots[[6]], plots[[4]])

plots[[1]] <- plots[[1]] +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = c(.8, .25)
  ) + geom_vline(xintercept = mean(all_lambdas[[1]])) +
  coord_cartesian(xlim = c(xmax, xmin))
plots[[2]] <- plots[[2]] +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none") + geom_vline(xintercept = mean(all_lambdas[[1]])) +
  coord_cartesian(xlim = c(xmax, xmin))
plots[[3]] <- plots[[3]] +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none") + geom_vline(xintercept = mean(all_lambdas[[1]])) +
  coord_cartesian(xlim = c(xmax, xmin))
plots[[4]] <- plots[[4]] +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none") + geom_vline(xintercept = mean(all_lambdas[[1]])) +
  coord_cartesian(xlim = c(xmax, xmin))
plots[[5]] <- plots[[5]] +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none") + geom_vline(xintercept = mean(all_lambdas[[1]])) +
  coord_cartesian(xlim = c(xmax, xmin))
plots[[6]] <- plots[[6]] +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none") + geom_vline(xintercept = mean(all_lambdas[[1]])) +
  coord_cartesian(xlim = c(xmax, xmin))

left_label <- textGrob("Coverage", gp = gpar(fontsize = 12), rot = 90)
bottom_label <- textGrob(expression(lambda), gp = gpar(fontsize = 12))

suppressMessages({
  pdf("./fig/distribution_of_beta.pdf", width = 7.5)
  g <- grid.arrange(grobs = plots, ncol = 3, nrow = 2, left =left_label, bottom = bottom_label)
  dev.off()
})

