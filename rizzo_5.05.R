n = 1000

mc.pnorm = function (x) {
  g = function (t) {
    x * exp(-(x*t)^2/2)
  }
  
  gs = g(runif(n))
  var = var(gs)/n
  zero.to.x = 1/sqrt(2*pi) * mean(gs)
  # use the property that the normal density funciton integrates to 1.
  theta.hat  = 2 * zero.to.x + (1 - 2*zero.to.x) / 2
  return(data.frame(theta.hat, var))
}

hit.miss.pnorm = function (p) {
  xs = rnorm(n) <= p
  var = var(xs)/n
  theta.hat = mean(xs)
  return(data.frame(theta.hat, var))
}

ps = seq(0, 1, 0.1)
mc = data.frame(t(sapply(ps, function (p) mc.pnorm(p))))
hit.miss = data.frame(t(sapply(ps, function (p) hit.miss.pnorm(p))))
truth = sapply(ps, function (p) pnorm(p))

mc.mean = unlist(mc$theta.hat)
hit.miss.mean = unlist(hit.miss$theta.hat)
round(rbind(ps, mc.mean, hit.miss.mean, truth), 3)
# mc is close to the truth than hit or miss.

(var.ratio = round(unlist(mc$var) / unlist(hit.miss$var), 3))
# mc is more efficient than hit miss.