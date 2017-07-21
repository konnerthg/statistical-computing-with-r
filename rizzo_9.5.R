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

posterior2= function (x, win) {
  if (x < b.lim[1] || x > b.lim[2]) {
    return(0)
  }
  nums = sapply(split(1:days, rep(1:(length(win)), days/length(win))), function(n) prod(n))
  dens = sapply(win, function(w) factorial(w))
  probs = prob.vector(x) ^ win
  return(prod(nums/dens*probs))
}

posterior = function (x, win) {
  if (x < b.lim[1] || x > b.lim[2]) {
    return(0)
  }

  # reduce factorial in the numerator as much as possible by subtracting the largest x_i from the factorial argument.
  i = which.max(win)
  win.max = win[i]
  win.tmp = win[-i]
  # avoid problems with factorial(0)
  win.tmp[win.tmp == 0] = 1
  
  res = 1
  # all factors smaller than 1
  smaller.than.1 = c(1/unlist(sapply(win.tmp, function (x) 1:x)), prob.vector(x)^win)
  # all factors larger than 1
  larger.than.1 = (win.max+1):days
  
  enlarge = TRUE
  enlarge.i = 1
  enlarge.lim = 1e200

  diminish = FALSE
  diminish.i = 1
  diminish.lim = 1e-200
  
  # attempt to multiply all large factors when the current result is small and vice-versa.
  while(enlarge || diminish) {
    #print('here')
    #print(res)
    #print(enlarge)
    #print(diminish)
    while(enlarge) {
      #print(enlarge.i)
      if (enlarge.i > length(larger.than.1)) {
        enlarge = FALSE
        if (diminish.i <= length(smaller.than.1)) {
          diminish = TRUE
        }
      } else {
        res = res * larger.than.1[enlarge.i]
        enlarge.i = enlarge.i + 1
        if (res >= enlarge.lim) {
          enlarge = FALSE
          diminish = TRUE
        }
      }
    }
    while(diminish) {
      if (diminish.i > length(smaller.than.1)) {
        diminish = FALSE
        if (enlarge.i <= length(larger.than.1)) {
          enlarge = TRUE
        }
      } else {
        res = res * smaller.than.1[diminish.i]
        diminish.i = diminish.i + 1
        if (res <= diminish.lim) {
          diminish = FALSE
          enlarge = TRUE
        }
      }
    }
  }
  print(length(larger.than.1))
  print(length(smaller.than.1))
  print(enlarge.i)
  print(diminish.i)
  
  return(res)
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
    # num = prob(y, win)
    # den = prob(xt, win)
    r = prob.ratio(y, xt, win)
    # TODO: the proabilities are very small. 
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
  plot(is, xb, type="l")
}