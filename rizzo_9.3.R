theta = 1
eta = 0
N = 10000

stopifnot(theta > 0)

df = function(x) {
  1/(theta*pi*(1+((x-eta)/theta)^2))
}

dg = function(x, df) {
  # dt(x = x, df = df)
  dnorm(x = x, mean = df)
}

rg = function(df) {
  rnorm(n = 1, mean = df)
  # rt(n = 1, df = df)
}

mh = function (N, df, dg, rg) {
  x = numeric(N)
  x[1] = rg(1)
  k = 0
  u = runif(N)
  for (i in 2:N) {
    xt = x[i-1]
    y = rg(xt)
    r = df(y) * dg(xt, y) / (df(xt) * dg(y, xt))
    if (u[i] <= r) {
      x[i] = y
    } else {
      k = k + 1
      x[i] = xt
    }
  }
  print(k)
  return(x)
}

x = mh(N, df, dg, rg)
is = 1001:N
par(mfrow = c(1,2))
plot(is, x[is], type="l")
hist(x, probability = TRUE, breaks = 100)
plot.x = seq(min(x), max(x), 0.01)
lines(plot.x, df(plot.x))
par(mfrow = c(1,1))