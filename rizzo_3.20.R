# ex 3.20

lambda = 3
shape = 6
scale = 2
size = 1000
eps = 1e-8

t = 10

n = qpois(1-eps, lambda = lambda * t)
ys = c(rgamma(n = n, shape = shape, scale = scale))

pp.exp = function (t0) {
  Tn = rexp(1000, lambda)
  Sn = cumsum(Tn)
  return(min(which(Sn > t0)) - 1)
}

ns = sapply(1:size, function (i) pp.exp(t))
xs = sapply(ns, function (n) {
  sum(ys[1:n])
})

# sample:
(mean.s = mean(xs))
(var.s = var(xs))

# theoretical:
(mean.t = lambda * t * shape * scale)
(var.t = (shape + 1) * shape * scale^2)