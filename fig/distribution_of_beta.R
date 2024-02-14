## Setup
source("./fig/setup/setup.R")

## Load Data
n <- 100
p <- 100
quantiles <- "zerosample2"
load(glue("{res_dir}/rds/distribution_of_beta_n{n}_p{p}_{quantiles}.rds"))
dist_types <- c("Sparse 1", "Sparse 2", "Sparse 3", "Normal", "Laplace", "T")

all_res[[1]]$dist_type <- dist_types[1]
all_res[[2]]$dist_type <- dist_types[2]
all_res[[3]]$dist_type <- dist_types[3]
all_res[[4]]$dist_type <- dist_types[4]
all_res[[5]]$dist_type <- dist_types[5]
all_res[[6]]$dist_type <- dist_types[6]

table(all_res[[1]]$group)
table(all_res[[1]]$truth)

plot_function <- function(plot_list) {

  lambda_max <- 1
  lambda_min <- 0.001
  lambda_seq <- 10^(seq(log(lambda_max, 10), log(lambda_min, 10), length.out = 10))

  plot_list <- plot_list %>%
    mutate(covered = truth >= lower & truth <= upper)

  overall_cov <- plot_list %>%
    dplyr::mutate(lambda = lambda_seq[lambda]) %>%
    dplyr::group_by(lambda) %>%
    dplyr::summarise(coverage = mean(covered)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(group = "Overall")
  size_cov <- plot_list %>%
    dplyr::mutate(lambda = lambda_seq[lambda]) %>%
    dplyr::mutate(group = as.character(group)) %>%
    dplyr::group_by(lambda, group) %>%
    dplyr::summarise(coverage = mean(covered)) %>%
    dplyr::ungroup()
  coverage_data <- dplyr::bind_rows(overall_cov, size_cov) %>%
    dplyr::mutate(group = factor(group, levels = c("Small", "Moderate", "Large", "Overall")))

  gg <- ggplot(data = coverage_data, aes(x = lambda, y = coverage, group = group, color = group)) +
    geom_line() +
    # geom_vline(xintercept = plot_list[[2]], linetype = "dashed", color = sec_colors[1], linewidth = .5) +
    geom_hline(yintercept = 0.8, linewidth = .5) +
    theme_bw() +
    scale_x_continuous(trans = log10_trans(),
                       breaks = trans_breaks('log10', function(x) 10^x),
                       labels = trans_format('log10', math_format(10^.x))) +
    coord_cartesian(xlim = c(1, .001), ylim = c(0, 1.0)) +
    scale_color_manual(name = expression(abs(beta)), values = colors) +
    ggtitle(plot_list$dist_type)

  return(gg)

}

plots <- lapply(all_res, plot_function)
plots <- list(plots[[1]], plots[[2]], plots[[3]], plots[[5]], plots[[6]], plots[[4]])

plots[[1]] <- plots[[1]] +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = c(.8, .25)
  ) # + geom_vline(xintercept = avg_lambdas[[1]])
plots[[2]] <- plots[[2]] +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none") # + geom_vline(xintercept = avg_lambdas[[2]])
plots[[3]] <- plots[[3]] +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none") # + geom_vline(xintercept = avg_lambdas[[3]])
plots[[4]] <- plots[[4]] +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none") # + geom_vline(xintercept = avg_lambdas[[4]])
plots[[5]] <- plots[[5]] +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none") # + geom_vline(xintercept = avg_lambdas[[5]])
plots[[6]] <- plots[[6]] +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none") # + geom_vline(xintercept = avg_lambdas[[6]])

left_label <- textGrob("Coverage", gp = gpar(fontsize = 12), rot = 90)
bottom_label <- textGrob(expression(lambda), gp = gpar(fontsize = 12))

suppressMessages({
  pdf("./fig/distribution_of_beta.pdf", width = 7.5)
  g <- grid.arrange(grobs = plots, ncol = 3, nrow = 2, left =left_label, bottom = bottom_label)
  dev.off()
})

