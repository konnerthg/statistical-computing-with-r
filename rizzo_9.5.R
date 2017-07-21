b = 0.2
b.lim = c(0, 0.5)
days = 250
secs = 1:5
m = 5000
burn = 1000
ws = c((1:4)/4)

prob.vector = function (b) {
  return(c(1/3, (1-b)/3, (1-2*b)/3, 2*b/3, b/3))
}

i = sample(secs, size = days, prob = ps, replace = TRUE)
win = tabulate(i)

ps = prob.vector(b)

# attempt to avoid numerical issues when computing the posterior density.
# will break when days is large enough.
posterior = function (x, win) {
  if (x < b.lim[1] || x > b.lim[2]) {
    return(0)
  }
  nums = sapply(split(1:days, rep(1:(length(win)), days/length(win))), function(n) prod(n))
  dens = sapply(win, function(w) factorial(w))
  probs = prob.vector(x) ^ win
  return(prod(nums/dens*probs))
}

# try to overcome numerical issues when computing acceptance probability in random walk.
prob.ratio = function (n, d, win) {
  return(prod(prob.vector(n)^win / prob.vector(d)^win))
}

rw.b = function (w) {
  x = numeric(m)
  
  u = runif(m)
  v = runif(m, -w, w)
  x[1] = w
  for (i in 2:m) {
    xt = x[i-1]
    y = xt + v[i]
    r = prob.ratio(y, xt, win)
    if (u[i] <= r) {
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
  hist(xb, breaks = 100, probability = TRUE, main = paste('w = ', ws[i], sep = ''))
  # TODO: failed to plot the posterior density.
  lines(xb.seq, sapply(xb.seq, function(x) posterior(x, win)))
  plot(is, xb, type="l")
}
par(mfrow = c(1,1))