# NOT ACTUAL PROCESS JUST SHOWS HOW RELATED
# Select prob (0 to 1)
# Find quantile value (highlight tail prob)
# Highlight total tail prob


## Setup
source("./fig/setup.R")

beta_seq <- seq(-0.5, 0.5, by = .001)
lambda <- 0.1
sigma2 <- 1
n <- 100
zjs <- c(0.2)

post_dens <- function(x, n, lambda, zj, sigma2) {
  if (x < 0) {
    exp((zj*lambda*n)/sigma2)*exp(-(n / (2*sigma2))*(x - (zj + lambda))^2)
  } else {
    exp(-(zj*lambda*n)/sigma2)*exp(-(n / (2*sigma2))*(x - (zj - lambda))^2)
  }
}

res <- list()
for (i in 1:length(zjs)) {
  dens <- sapply(beta_seq, post_dens, n, lambda, zjs[i], sigma2)
  # dens <- dens / max(dens)
  res[[i]] <- data.frame(x = beta_seq, y = dens, zj = zjs[i])
}
dat <- do.call(dplyr::bind_rows, res) %>%
  mutate(y = y / sum(y * 0.001))

xs1 <- seq(0, 0.5, by = 0.01)
norm_dens <- dnorm(xs1, mean = zjs - lambda, sd = sqrt(sigma2 / n))
xs2 <- seq(-0.5, 0, by = 0.01)
norm_dens2 <- dnorm(xs2, mean = zjs + lambda, sd = sqrt(sigma2 / n))
norm_dens_other <- dnorm(xs2, mean = zjs - lambda, sd = sqrt(sigma2 / n))
norm_dens2_other <- dnorm(xs1, mean = zjs + lambda, sd = sqrt(sigma2 / n))

true_dens <- dat %>% filter(x == 0) %>% pull(y)

ggplot2::ggplot() +
  ggplot2::geom_area(data = dat, aes(x = x, y = y), alpha = 0.5) +
  ggplot2::facet_wrap(~zj, labeller = label_bquote(z == .(zj))) +
  ggplot2::geom_line(data = data.frame(x = xs1, y = norm_dens), aes(x = x, y = y)) +
  ggplot2::geom_line(data = data.frame(x = xs2, y = norm_dens2), aes(x = x, y = y)) +
  ggplot2::geom_line(data = data.frame(x = xs1, y = norm_dens * (true_dens / norm_dens[1])), aes(x = x, y = y), color = "red") +
  ggplot2::geom_line(data = data.frame(x = xs2, y = norm_dens2 * (true_dens / norm_dens2[51])), aes(x = x, y = y), color = "red") +
  ggplot2::geom_line(data = data.frame(x = xs2, y = norm_dens_other), aes(x = x, y = y), linetype = "dashed") +
  ggplot2::geom_line(data = data.frame(x = xs1, y = norm_dens2_other), aes(x = x, y = y), linetype = "dashed") +
  theme_bw() +
  theme(panel.spacing = unit(1, "lines")) +
  xlab(expression(beta)) +
  ylab("Density")
