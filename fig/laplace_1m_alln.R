## Setup
source("./fig/setup/setup.R")

dlaplace <- function(x, rate = 1) {
  dexp(abs(x), rate) / 2
}

plots <- list()

method <- "zerosample2"
data_type <- "laplace"


corr <- "exchangeable"
rho <- 0

per_var_data <- list()
alpha <- .2

#load(glue("{res_dir}/rds/{data_type}_{corr}_rho{rho*100}_{method}_alpha{alpha*100}.rds"))
load(glue("{res_dir}/rds/{data_type}_{corr}_rho{rho*100}_{method}_alpha{alpha*100}_rt05.rds"))
# load(glue("{res_dir}/rds/{data_type}_{corr}_rho{rho*100}_{method}_alpha{alpha*100}_rt10.rds"))
# load(glue("{res_dir}/rds/laplace_zerosample2_alpha20_p40_1000.rds"))
per_var_data <- per_var

# cutoff <- 2
cutoff <- round(quantile(abs(per_var_data$truth), .98), 1)
ns <- unique(per_var_data$n)
ns <- c(30, 40, 80)
line_data <- list()
line_data_avg <- list()
for (j in 1:length(ns)) {

  tmp <- per_var_data %>%
    mutate(
      covered = lower <= truth & upper >= truth,
      mag_truth = abs(truth), covered = as.numeric(covered)
    ) %>%
    filter(n == ns[j])

  fit <- gam(covered ~ s(mag_truth) + s(group, bs = "re"), data = tmp, family = binomial)
  xs <- seq(0, cutoff, by = .01)
  ys <- predict(fit, data.frame(mag_truth = xs, group = 101), type ="response")

  line_data[[j]] <- data.frame(x = xs, y = ys, n = factor(ns[j]))
  line_data_avg[[j]] <- data.frame(avg = mean(tmp$covered), n = factor(ns[j]))
}

line_data_avg <- do.call(rbind, line_data_avg)
line_data <- do.call(rbind, line_data)

plt <- ggplot() +
  geom_line(data = line_data, aes(x = x, y = y, color = n)) +
  geom_hline(data = line_data_avg, aes(yintercept = avg, color = n), linetype = 2) +
  geom_hline(aes(yintercept = 1 - alpha), linetype = 1, alpha = .5) +
  theme_bw() +
  xlab(expression(abs(beta))) +
  ylab(NULL) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0, round(quantile(abs(per_var_data$truth), .98), 1))) +
  scale_color_manual(name = "N", values = colors)


# x1 <- seq(-.4, .4, by = 0.01)
# x2 <- seq(-2, 2, by = 0.01)
#
# y1 <- dlaplace(x1, rate = 10)
# y2 <- dlaplace(x2, rate = 2)
#
# x1 <- x1 / .4
# x2 <- x2 / 2
# y1 <- y1 / max(y1)
# y2 <- y2 / max(y2)
#
# plot(x1, y1, type = "l", lty = 2)
# lines(x2, y2)

# suppressMessages({
pdf("./fig/laplace_1m_alln.pdf", height = 3.5)
plt
dev.off()
# })

