# Sparse 1: 1-10 = ±(0.5, 0.5, 0.5, 1, 2), 11−100 = 0; 0.09
# Sparse 2: 0.12
# Sparse 3: 0.20
# Laplace: 0.25
# T: 0.55
# Normal: 0.40
# Unif: 1.5
dlaplace <- function(x, rate = 1) {
  dexp(abs(x), rate) / 2
}
integrate(function(x) x*dnorm(x), 0, Inf)$val
