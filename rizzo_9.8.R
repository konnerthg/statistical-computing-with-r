n = 100
a = 30
b = 60

df = function (x, y) {
  # general binomial coefficient
  gamma(n + 1) / (gamma(x + 1) * gamma(n - x + 1))  * y^(x + a - 1) * (1 - y)^(n - x + b - 1)
}

m = 10000
d = 2

x = matrix(0, nrow = m, ncol = d)

for (i in 2:m) {
  xt = x[i-1,]
  xt[1] = rbinom(1, n, xt[2])
  xt[2] = rbeta(1, xt[1] + a, n - xt[1] + b)
  x[i,] = xt
}

plot(x, cex = 0.1)
xs = seq(from = min(x[,1]), to = max(x[,1]), length.out = 200)
ys = seq(from = min(x[,2]), to = max(x[,2]), length.out = 200)
zs = t(sapply(xs, function (x) sapply(ys, function (y) df(x, y))))

# plot contour of density for verification.
contour(xs, ys, zs, add = TRUE, col = 2)