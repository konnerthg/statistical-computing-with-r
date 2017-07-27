a = c(1,2)
d = length(a)

getTerm = function (a, k) {
  d = length(a)
  return((-1)^k * exp((2*k+2)*log(norm(a, type = "2")) - lgamma(k+1) - k*log(2) - log(2*k + 1) - log(2*k + 2) + lgamma((d+1)/2) + lgamma(k + 3/2) - lgamma(k + d/2 + 1)))
}

n = 400

getSum = function (a) {
  sum(sapply(0:n, function (k) getTerm(a, k)))
}