# WIP: doesn't work.

n1 = 100
n2 = 200

# returns probability for H1, where H0 is equal variances (i.e. sd1 = sd2)
test.equal.variance = function(xs1, xs2){
  # let x1 be smaller than x2
  if (length(x2) < length(x1)) {
    tmp = x1
    x1 = x2
    x2 = tmp
  }
  n1 = length(x1)
  n2 = length(x2)
  
  y = c(x1, x2)
  
  # returns 1 if more than five values from one sample lie outside the range of the other sample.
  # this implies different varianes between the samples.
  count5 = function (x1, x2) {
    stopifnot(length(x1) == length(x2))
    extr1 = sum((x1 < min(x2))) + sum((x1 > max(x2)))
    extr2 = sum((x2 < min(x1))) + sum((x2 > max(x1)))
    
    out = max(extr1, extr2)
    return(as.integer(out > 5))
  }
  
  # run permutation test.
  # if H0 holds, count 5 must return 0 for "most" (depending on desired p-value) sets of 2 equally sized samples from y.
  rep = 1000
  n.min = n1
  n = length(y)
  c5s = numeric(rep)
  size = floor(n / 2)
  
  for (i in 1:rep) {
    k = sample(1:n, size, replace = FALSE)
    y1 = y[k]
    y2 = y[-k]
    # count how many times count 5 test returned 1 (i.e. differend variances)
    c5s[i] = count5(y1, y2)
  }
  
  hist(c5s, probability = TRUE)
  
  # frequency of different variances according to count 5 test.
  return(mean(c5s))
}

mean = 0
# test the algorithm for random normally distributed samples of unequal sizes with different chocies of the standard deviation.
test.equal.variance(rnorm(n1, mean, 1), rnorm(n2, mean, 1))
test.equal.variance(rnorm(n1, mean, 10), rnorm(n2, mean, 1))
test.equal.variance(rnorm(n1, mean, 100), rnorm(n2, mean, 1))