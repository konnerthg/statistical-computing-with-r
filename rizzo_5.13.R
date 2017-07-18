g = function (x) {
  x ^ 2 / sqrt(2*pi) * exp(-x^2/2)
}

xs = seq(0,10,0.1)

ys.g = g(xs)
ys.rayleigh = drayleigh(xs, sigma = 1.5)
ys.norm = dnorm(xs, mean = 1.5)
lim = max(c(ys.g, ys.rayleigh, ys.norm))

plot(xs, ys.g, type = "l", ylim = c(0, lim))
lines(xs, ys.rayleigh, col="red", ylim = c(0, lim))
lines(xs, ys.norm, col="blue", ylim = c(0, lim))

# f1(x) = drayleigh(x, sigma = 1.5)
# f2(x) = dnorm(x, mean = 1.5)

# f2 is a little closer to g. should be better.