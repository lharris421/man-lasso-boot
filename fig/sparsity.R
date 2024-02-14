dlaplace <- function(x, rate = 1) {
  dexp(abs(x), rate) / 2
}

sparse_beta <- c(rep(-2, 1), rep(-1, 1), rep(-0.5, 3), rep(2, 1), rep(1, 1), rep(0.5, 3), rep(0, 90))

xs <- seq(-4, 4, by = 0.01)
y1 <- dlaplace(xs); y1 <- y1 / max(y1)
y2 <- dnorm(xs); y2 <- y2 / max(y2)
y3 <- dt(xs, 3); y3 <- y3 / max(y3)
y4 <- dnorm(xs) * .3 + (xs == 0) * .7; y4 <- y4 / max(y4)
y5 <- dnorm(xs) * .5 + (xs == 0) * .5; y5 <- y5 / max(y5)
y6 <- sapply(xs, function(x) mean(abs(x - sparse_beta) < 1e-3)); y6 <- y6 / max(y6)

xs <- seq(-4, 4, by = 0.01)
y1 <- dlaplace(xs)
y2 <- dnorm(xs)
y3 <- dt(xs, 3)
y4 <- dnorm(xs) * .3 + (xs == 0) * .7
y5 <- dnorm(xs) * .5 + (xs == 0) * .5
y6 <- sapply(xs, function(x) mean(abs(x - sparse_beta) < 1e-3))


pdf("./fig/sparsity.pdf", height = 3)
plot(xs, y1, type = "l", col = "green", ylim = c(0, 1))
lines(xs, y2, col = "red")
lines(xs, y3, col = "blue")
lines(xs, y4, col = "purple")
lines(xs, y5, col = "yellow")
lines(xs, y6, col = "brown")
dev.off()
