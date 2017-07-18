n = 10000
a = 3
b = 3

# plot the density to get an idea of the problem.
xs = rbeta(n, shape1 = a, shape2 = b)
plot(density(xs))

mc.cdf.beta = function (p, shape1, shape2) {
  if (p <= 0 || p >= 1) return(0)
  us = runif(n)
  return(mean(dbeta(p*us, a, b)) * p)
}

xs = seq(0, 1, 0.1)

round(rbind(sapply(xs, function (x) mc.cdf.beta(x, a, b)), sapply(xs, function(x) pbeta(x, a, b))), 3)