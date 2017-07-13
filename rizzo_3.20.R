# ex 3.20

lambda = 1
shape = 7.5
scale = 1
size = 1000

pp.exp = function (t0, lambda) {
  Tn = rexp(size, lambda)
  Sn = cumsum(Tn)
  return(min(which(Sn > t0)) - 1)
}

comp.pois = function(t.max, lambda) {
  stopifnot(t.max >= 0 && t.max %% 1 == 0)
  # offset ns by 1 because first y is 0.
  ns = cumsum(rpois(n = t.max, lambda = lambda)) + 1
  # ns = sapply(1:t.max, function (t) pp.exp(t, lambda)) + 1
  ys = c(0, rgamma(n = max(ns), shape = shape, scale = scale))
  return(c(0, cumsum(x = ys[ns])))
}

# prepend 0 to cover cases where N(t) = 0.

t.max <- 20
xs = comp.pois(t.max = t.max, lambda = lambda)
plot(y = xs, x = 0:t.max, ylab = "X(t)", xlab = "t")

# low variance in the gamma distribution yields a linear looking plot.
# a low lambda results in more steps.

t = 100
ts = sapply(1:size, function(i) comp.pois(t, lambda)[t])

# sample:
(mean.s = mean(ts))
(var.s = var(ts))

# theoretical:
(mean.t = lambda * t * shape * scale)
# https://ocw.mit.edu/courses/mathematics/18-443-statistics-for-applications-fall-2006/lecture-notes/lecture6.pdf
(var.t = (shape + 1) * shape * scale^2)