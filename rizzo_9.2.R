sigma = 2

dg = function (a, b) {
  dgamma(x = a, shape = b, rate = 1)
}

rg = function (s) {
  rgamma(1, shape = s, rate = 1)
}

df = function (x) {
  drayleigh(x, sigma = sigma)
}

mh = function(rg, dg, df) {
  N = 10000
  x = numeric(N)
  x[1] = rg(1)
  u = runif(N)
  k = 0
  for (i in 2:N) {
    xt = x[i-1]
    y = rg(xt)
    r = df(y)*dg(xt, y) / (df(xt)*dg(y, xt))
    if (u[i] <= r) {
      x[i] = y
    } else {
      x[i] = x[i-1]
      k = k + 1
    }
  }
  print(k)
  return(x)
}

x = mh(rg, dg, df)
is = 5000:5500
plot(is, x[is], type="l")