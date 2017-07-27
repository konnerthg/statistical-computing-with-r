findIntersection = function (k) {
  s.k.minus.one = function (a) {
    1-pt(sqrt(a^2 * (k - 1) / (k - a^2)), df = k-1)
  }
  
  s.k = function (a) {
    1-pt(sqrt(a^2 * k / (k + 1 - a^2)), df = k)
  }
  
  f = function (a) {
    s.k(a) - s.k.minus.one(a)
  }
  
  eps = 1e-2
  return(uniroot(f, interval = c(eps, sqrt(k)-eps))$root)
}

rs = sapply(c(4:25, 100, 500, 1000), function (k) {
  findIntersection(k)
  })
