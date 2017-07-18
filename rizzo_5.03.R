m = 10000

estimate.unif = function () {
  g = function (y) {
    exp(-y/2) * 1/2
  }
  xs = g(runif(m))
  theta.hat = mean(xs)
  var = var(xs)/m
  return(data.frame(theta.hat, var))
}

estimate.unif.min.max = function () {
  g = function (y) {
    exp(-y)
  }
  # divide by 2, since dunif(x, 0, 0.5) = 2 for x in (0, 0.5).
  xs = g(runif(m, min = 0, max = 0.5))
  var = var(xs)/m
  
  theta.hat = mean(xs) * 1/2
  return(data.frame(theta.hat, var))
}

estimate.exp = function () {
  # theta.hat = pexp(0.5, rate = 1) - pexp(0, rate = 1)
  g = function (y) {
    1/y
  }
  
  y = rexp(m, rate = 1) <= 0.5
  var = var(y)/m
  
  theta.hat = mean(y)
  return(data.frame(theta.hat, var))
}

estimate.unif()
estimate.unif.min.max()
estimate.exp()

# exp estimator samples of 0 and 1. therefore, higher variance.