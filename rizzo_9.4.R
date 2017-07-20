sds = c(0.5, 1, 2, 4)
is = 5000:5500

mc.laplace = function (sd) {
  N = 10000
 
  # standard laplace distribution.
  df = function (x) {
    1/2 * exp(-abs(x))
  }
  
  rg = function (mean) {
    rnorm(n = 1, mean = mean, sd = sd)
  }
  
  rw = function (N, df, rg) {
    
    x = numeric(N)
    x[1] = rg(1)
    k = 0
    us = runif(N)
    
    for (i in 2:N) {
      xt = x[i-1]
      y = rg(xt)
      res = df(y) / df(xt)
      if (us[i] <= res) {
        x[i] = y
      } else {
        x[i] = xt
        k = k + 1
      }
    }
    print(k)
    
    return(x)
  }
  
  return(rw(N, df, rg))
}

xs = mc.laplace(sd = sds[1])
par(mfrow=c(length(sds), 1))
plot(is, xs[is], type="l", ylab = paste("sd = ", sds[1], sep = ''))
for (i in 2:length(sds)) {
  plot(is, mc.laplace(sd = sds[i])[is], type="l", ylab = paste("sd = ", sds[i], sep = ''))
}
par(mfrow=c(1, 1))