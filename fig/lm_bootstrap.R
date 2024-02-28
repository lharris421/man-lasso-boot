## Setup
source("./fig/setup/setup.R")

## Parameters
data_type <- "normal"
sd <- 1
alpha <- .2
args_list <- list(data = "normal",
                  n = c(200, 500, 1000),
                  snr = 1,
                  sd = ifelse(data_type == "normal", sd, NA),
                  rate = ifelse(data_type == "laplace", rt, NA),
                  a = ifelse(data_type == "abn", a, NA),
                  b = ifelse(data_type == "abn", a, NA),
                  correlation_structure = "exchangeable",
                  correlation = 0,
                  correlation_noise = ifelse(data_type == "abn", rho.noise, NA),
                  method = "lm",
                  ci_method = "quantile",
                  nominal_coverage = alpha * 100,
                  modifier = NA,
                  lambda = "cv",
                  p = 100)

args_grid <- expand.grid(args_list)
partial_res <- list()
for (i in 1:length(args_list$n)) {
  read_objects(rds_path, args_grid[i,])
  partial_res[[i]] <- do.call(rbind, res) %>%
    mutate(covered = lower <= truth & upper >= truth, group = rep(1:args_list$p, each = 100), n = args_grid[i,"n"])
}

pdat <- do.call(rbind, partial_res) %>%
  mutate(n = as.factor(n)) %>%
  group_by(group, n) %>%
  summarise(coverage = mean(covered))

pdf("./fig/lm_bootstrap.pdf", height = 4, width = 5)
ggplot(pdat, aes(y = coverage, group = n, fill = n)) +
  geom_boxplot() +
  geom_hline(yintercept = 1 - alpha, col = "red") +
  theme_bw() + ylab("Coverage") +
  ggtitle(glue("Bootstrap CI Coverage, p = {args_list$p}"), subtitle = expression(beta %~% N(0, 1))) +
  scale_color_manual(values = colors)
dev.off()
