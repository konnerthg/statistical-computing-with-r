# change of variables yields integral from 0 to 1 of sin(x pi/3)  pi/3 dx.

n = 10000

g = function (x) {
  sin(pi/3 * x) * pi / 3
}

(theta.hat = mean(g(runif(n))))
(theta = 1/2)