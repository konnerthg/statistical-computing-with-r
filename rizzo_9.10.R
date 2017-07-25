library(coda)

l = 10
s = c(1,2,3,4)
k = length(s)

# rayleigh density.
sigma = 4
df = function (y, sigma) {
  if (any(x < 0)) return(0)
  stopifnot(sigma > 0)
  return(y/sigma^2 * exp(-y^2/(2*sigma^2)))
}

dg = function (y, xt) {
  dchisq(y, df = xt)
}

rg = function (xt) {
  rchisq(1, df = xt)
}

mh = function (s, l) {
  x = numeric(l)
  us = runif(l)
  x[1] = s
  for (i in 2:l) {
    xt = x[i-1]
    y = rg(xt)
    res = df(y, sigma)/df(xt, sigma) * dg(xt, y)/dg(y, xt)
    if (us[i] <= res) {
      x[i] = y
    } else {
      x[i] = xt
    }
  }
  return(x)
}

xs = matrix(sapply(1:k, function(i) mh(s[i], l)), nrow = k, byrow = TRUE)

# visualize the generated samples.
plotHists = function () {
  par(mfrow=c(1,k))
  for (i in 1:k) {
    hist(xs[i,], probability = TRUE, breaks = 100)
    x.axis = seq(min(xs[i,]), max(xs[i,]), by = 0.01)
    lines(x.axis, df(x.axis, sigma))
  }
  par(mfrow=c(1,1))
}

plotChains = function () {
  burn = 2000
  is = (burn+1):l
  par(mfrow=c(1,k))
  for (i in 1:k) {
    plot(is, xs[i,is], type="l")
  }
  par(mfrow=c(1,1))
}

gelman.rubin = function (psis) {
  psi.means = rowMeans(psis)
  B = n * var(psi.means)
  W = mean(apply(psis, MARGIN = 1, "var"))
  var.hat = (n-1)/n*W + 1/n*B
  return(var.hat/W)
}

div = matrix(sapply(1:k, function(i) 1:l), nrow = k, byrow = TRUE)
psis = t(apply(xs, MARGIN = 1, "cumsum")) / div

r.hats = sapply(2:l, function(j) gelman.rubin(psis[,1:j]))

plot(2:l, r.hats, type="l")
abline(h=1.2)

# TODO: use coda library.