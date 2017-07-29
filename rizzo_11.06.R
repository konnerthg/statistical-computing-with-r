my.dcauchy = function (x, eta, theta) {
  stopifnot(theta > 0)
  return(1/(theta*pi*(1 + ((x - eta)/theta)^2)))
}

my.pcauchy = function (x, eta, theta) {
  stopifnot(theta > 0)
  
  integral = function (x) {
    my.dcauchy(x, eta, theta)
  }
  
  return(integrate(integral, lower = -Inf, upper = x)$value)
}

eta = 0
theta = 2
xs = seq(-10, 10)
estimate = sapply(xs, function(x) my.pcauchy(x, eta, theta))
truth = sapply(xs, function(x) pcauchy(x, eta, theta))
round(rbind(estimate, truth), 4)