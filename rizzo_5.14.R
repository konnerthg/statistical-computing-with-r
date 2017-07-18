g = function (x) {
  x ^ 2 / sqrt(2*pi) * exp(-x^2/2) * (x > 1)
}

sigma.rayleigh = 1.5
mean = 1.5
n = 10000

f1 = function (x) {
  drayleigh(x, sigma = sigma.rayleigh) * (x > 1)
}

f2 = function (x) {
  dnorm(x, mean = mean) * (x > 1)
}

rf1 = function () {
  rrayleigh(n, sigma = sigma.rayleigh)
}

rf2 = function () {
  rnorm(n, mean = mean)
}

is.rayleigh = function () {
  xs = rf1()
  return(mean(g(xs)/f1(xs), na.rm = TRUE))  
}

is.norm = function () {
  xs = rf2()
  return(mean(g(xs)/f2(xs), na.rm = TRUE))  
}

(theta1 = is.rayleigh())
(theta2 = is.norm())
(truth = 0.400626)