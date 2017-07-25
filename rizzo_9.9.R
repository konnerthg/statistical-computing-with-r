sigmas = c(0.2, 0.5, 2)
s = c(-10, -5, 5, 10) # starting values.
l = 15000 # length of one chain.
k = length(s) # number of chains.

getMHSample = function (sigma) {
  
  # metropolis-hastings sampler to generate a normally distributed sample.
  mh = function(s, l, sd) {
    df = function(y) {
      return(dnorm(y))
    }
    
    rg = function(xt) {
      return(rnorm(n = 1, mean = xt, sd = sd))
    }
    
    dg = function(y, xt) {
      return(dnorm(x = y, mean = xt, sd = sd))
    }
    
    xs = numeric(l)
    xs[1] = s
    us = runif(l)
    for(i in 2:l) {
      xt = xs[i-1]
      y = rg(xt)
      res = df(y) / df(xt) * dg(xt, y) / dg(y, xt)
      if (us[i] <= res) {
        xs[i] = y
      } else {
        xs[i] = xt
      }
    }
    return(xs)
  }
  
  xs = matrix(sapply(s, function(i) mh(i, l, sigma)), nrow = 4, byrow = TRUE)
  return(xs)
}

makePlots = function(sigma) {
  xs = getMHSample(sigma)
  
  # avoid re-computing diagnostic statistics when plotting R-hat.
  getPhis = function (xs) {
    # compute means efficiently.
    phis = t(apply(xs, MARGIN = 1, cumsum))
    div = matrix(sapply(1:nrow(phis), function(r) 1:ncol(phis)), nrow = nrow(phis), byrow = TRUE)
    return(phis/div)
  }
  
  # the rows of X are generated chains.
  gelman.rubin = function (phis) {
    row.means = rowMeans(phis)
    phi.mean = mean(row.means)
    
    B = n * var(row.means)
    
    W = mean(apply(phis, 1, "var"))
    var.hat = (n-1)/n * W + 1/n * B
    return(var.hat/W)
  }
  
  phis = getPhis(xs)
  
  # why omit the plots? i like the plots.
  # plot sequence of phis.
  burn = 2000
  is = (1:l)[(burn+1):l]
  for(i in 1:k) {
    plot(is, phis[i,is], type="l", ylab = paste("sigma = ", sigma, ", x[1] = ", s[i], sep=''))
  }
  
  r.hats = sapply((burn + 1):l, function(j) gelman.rubin(phis[,1:j]))
  
  # plot sequence of r.hats.
  plot(is, r.hats, type="l")
  abline(h = 1.2)
}


par(mfrow = c(length(sigmas), 5))
sapply(sigmas, function(sigma) makePlots(sigma))
par(mfrow = c(1, 1))

