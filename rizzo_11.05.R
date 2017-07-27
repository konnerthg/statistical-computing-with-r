solve.equation = function (k) {
  
  expr.integral = function(u, n) {
    (1 + u^2/(n-1))^(-n/2)
  }
  
  c = function (n, a) {
    sqrt(a^2 * n / (n + 1 - a^2))
  }
  
  expr = function (n, a) {
    
    this.integral = function (u) {
      expr.integral(u, n)
    }
    
    c.n = c(n - 1)
    
    2/sqrt(pi*(n-1)) * exp(lgamma(n/2)-lgamma((n-1)/2)) * integrate(this.integral, lower = 0, upper = c.n)
  }
  
  f = function (a) {
    left = expr(k, a)
    right = expr(k + 1, a)
    return (left - right)
  }
  
  eps = 1e-2
  return(uniroot(f, interval = c(eps, sqrt(k)-eps))$root)
}

rs2 = sapply(c(4:25, 100, 500, 1000), function (k) {
  solve.equation(k)
})