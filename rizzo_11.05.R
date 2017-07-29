# the two sides of the equation are the same expression for a different k. i try to simplify the task by writing them as functions.
solve.equation = function (k) {

  # general integral.  
  expr.integral = function(u, n) {
    (1 + u^2/(n-1))^(-n/2)
  }
  
  # general c_k.
  get.c = function (n, a) {
    sqrt(a^2 * n / (n + 1 - a^2))
  }
  
  # left or right side of the equation, depending wheather n = k or n = k + 1.
  expr = function (n, a) {
    
    this.integral = function (u) {
      expr.integral(u, n)
    }
    
    c = get.c(n - 1, a)
    
    2/sqrt(pi*(n-1)) * exp(lgamma(n/2)-lgamma((n-1)/2)) * integrate(this.integral, lower = 0, upper = c)$value
  }
  
  f = function (a) {
    left = expr(k, a)
    right = expr(k + 1, a)
    return (left - right)
  }
  
  eps = 1e-2
  if (f(eps) < 0 && f(sqrt(k) - eps) > 0 || f(eps) > 0 && f(sqrt(k) - eps) < 0) {
    r = uniroot(f, interval = c(eps, sqrt(k)-eps))$root
  } else {
    r = NA
  }
  return(r)
}

rs2 = sapply(c(4:25, 100, 500, 1000), function (k) {
  solve.equation(k)
})


# plot f for a fixed k for debugging.
# xs = seq(eps, sqrt(k)-eps, 0.01)
# plot(xs, sapply(xs, function(x) f(x)), type="l")