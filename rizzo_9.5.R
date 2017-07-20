b = 0.2
b.lim = c(0, 0.5)
days = 250
secs = 1:5
m = 5000
burn = 0
ws = c((1:2)/4)

rw.b = function (w) {
  x = numeric(m)
  
  y.from.b = function (b) {
    return(c(1/3, (1-b)/3, (1-2*b)/3, 2*b/3, b/3))
  }
  
  ps = y.from.b(b)
  
  i = sample(secs, size = days, prob = ps, replace = TRUE)
  win = tabulate(i)
  
  prob = function (x, win) {
    if (x < b.lim[1] || x > b.lim[2]) {
      return(0)
    }
    return(prod(y.from.b(x)^win))
  }
  
  u = runif(m)
  v = runif(m, -w, w)
  x[1] = w
  for (i in 2:m) {
    xt = x[i-1]
    y = xt + v[i]
    if (u[i] <= prob(y, win)/prob(xt, win)) {
      x[i] = y
    } else {
      x[i] = xt
    }
  }
  
  return(x)
}

xbs = lapply(ws, function(w) rw.b(w))
par(mfrow = c(length(ws), 2))
is = (burn+1):m
for (i in 1:length(ws)) {
  xb = xbs[[i]]
  xb = xb[is]
  xb.seq = seq(min(xb), max(xb), 0.05)
  hist(xb, breaks = 100, probability = TRUE)
  plot(is, xb[is], type="l")
}