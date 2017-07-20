# ex 3.20

lambda = 3
shape = 6
scale = 2
size = 10000
eps = 1e-8

t = 10

# with probability 1-eps, n or less gamma distributed random variables are needed.
n = qpois(1-eps, lambda = lambda * t)
# sample from the gamma distribution. Not sure if it's ok to use the same sample every time.
# with this, the mean is of by about 10%.
# ys = c(rgamma(n = n, shape = shape, scale = scale))

# the interarrival times are exponentially distributed with rate lambda.
pp.exp = function (t0) {
  # not sure how many Tn are needed :/
  Tn = rexp(1000, lambda)
  Sn = cumsum(Tn)
  return(min(which(Sn > t0)) - 1)
}

# generate N(t) which follow the poisson process.
ns = sapply(1:size, function (i) pp.exp(t))
# generate X(t) as in the problem description.
xs = sapply(ns, function (n) {
  ys = c(rgamma(n = n, shape = shape, scale = scale))
  sum(ys[1:n])
})

# compare mean and variance of 'size' samples of X(t) for verification.
# sample:
(mean.s = mean(xs))
(var.s = var(xs))

# theoretical:
(mean.t = lambda * t * shape * scale)
(var.t = (shape + 1) * shape * scale^2)