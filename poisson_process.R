lambda = 2
t0 = 3
size = 10000

pp.exp = function (t0) {
  Tn = rexp(100, lambda)
  Sn = cumsum(Tn)
  return(min(which(Sn > t0)) - 1)
}

n1s = sapply(1:size, function () pp.exp(t0))
n2s = rpois(size, t0 * lambda)

rbind(summary(n1s), summary(n2s))