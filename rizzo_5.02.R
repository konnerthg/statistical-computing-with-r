x = seq(-10, 10, 2)
n = 10000

mc.norm = function (x) {
  g = function (t) {
    x * exp(-(x*t)^2/2)
  }

  gs = g(runif(n))

  var = var(gs) / n
  
  se = sqrt(var)
      
  zero.to.x = 1/sqrt(2*pi) * mean(gs)
  
  # use the property that the normal density funciton integrates to 1.
  theta.hat  = 2 * zero.to.x + (1 - 2*zero.to.x) / 2
  
  # E(theta.hat) = theta.
  # 95% CI for N(0,1) is (-1.96, 1.96).
  # by the CLT (theta.hat - theta) / se ~ N(0,1).
  CI = theta.hat + 1.96 * se
  
  return(data.frame(theta.hat, var, CI))
}
