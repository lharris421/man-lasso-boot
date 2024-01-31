## Setup
source("./fig/setup/setup.R")

## Load Data
n <- 50
p <- 50
method <- "quantile"
quantiles <- "zerosample"
dist <- "laplace"
load(glue("{res_dir}/rds/overall_v_feature_{dist}_{quantiles}_{method}_n{n}_p{p}.rds"))

coverages <- coverages %>%
  dplyr::mutate(mag_truth = abs(truth),
                covered = lower <= truth & upper >= truth,
                non_zero = lower > 0 | upper < 0)

## GLMM
fit <- gam(covered ~ s(mag_truth) + s(group, bs = "re"), data = coverages, family = binomial)
xs <- seq(0, 3, by = .01)
ys <- predict(fit, data.frame(mag_truth = xs, group = 101), type ="response")

## Density of abs(beta)
dens <- density(abs(coverages$truth), bw = .1, n = 301, from = 0, to = 3)

## Mean of (GLMM * Density)
sum(ys * dens$y) / sum(dens$y)

## Plot data
pdat <- data.frame(x = xs, coverage = ys, density = dens$y)
gg <- ggplot(pdat) +
  geom_area(aes(x = x, y = density / max(density)), fill = "grey", alpha = 0.5) +
  geom_line(aes(x = x, y = coverage), color = colors[2]) +
  geom_hline(yintercept = sum(ys * dens$y) / sum(dens$y)) +
  xlab(expression(abs(beta))) + ylab("Relative Density / Predicted Coverage")


#suppressMessages({
  pdf("./fig/overall_v_feature.pdf", height = 5)
  gg
  dev.off()
#})
