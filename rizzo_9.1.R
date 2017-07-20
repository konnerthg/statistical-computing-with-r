library(VGAM)

mc.drayleigh = function (sigma) {
  
  dg = function (a, b) {
    dchisq(a, df = b)
  }
  
  rg = function (df) {
    rchisq(n = 1, df = df)
  }
  
  df = function (x) {
    drayleigh(x, sigma = sigma)
  }
  
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

is = 5000:5500
x1 = mc.drayleigh(4)
x2 = mc.drayleigh(2)
par(mfrow=c(2,1))
plot(is, x1[is], type="l")
plot(is, x2[is], type="l")
par(mfrow=c(1,1))
# less efficient for sigma = 2. longer horizontal paths in the respective plot.