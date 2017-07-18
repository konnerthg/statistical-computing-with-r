n = 100

mc.skewness = function(xs) {
  
  # https://en.wikipedia.org/wiki/Skewness#Sample_skewness
  sample.skewness = function (sample) {
    mu = mean(sample)
    n = length(sample)
    num = 1/n * sum(sapply(sample, function (x) (x - mu)^3))
    denom = sd(sample)^3
    return (num/denom)
  }
  
  theta.hat = sample.skewness(xs)
  
  B = 200
  theta.hats.b = numeric(B)
  
  for (b in 1:B) {
    i = sample(1:n, n, TRUE)
    xs.b = xs[i]
    theta.hats.b[b] = sample.skewness(xs.b)
  }
  
  sd.hat = sd(theta.hats.b)
  
  # visualize.
  par(mfrow = c(1, 1))
  hist(theta.hats.b)
  
  # confidence intervals.
  alpha = 0.05
  probs = c(alpha/2, 1-alpha/2)
  names = sapply(probs, function (p) paste(p*100, '%', sep = ''))
  qs.theta.hats.b = quantile(theta.hats.b, probs)
  
  # standard normal.
  qs.norm = qnorm(probs)
  ci.norm = rev(theta.hat - qs.norm * sd.hat)
  
  # basic bootstrap.
  ci.basic = rev(2*theta.hat - qs.theta.hats.b)
  
  # percentile.
  ci.percentile = qs.theta.hats.b
  
  ci.data = data.frame(rbind(ci.norm, ci.basic, ci.percentile))
  colnames(ci.data) = names
  ci.data['left.miss'] = 0
  ci.data['right.miss'] = 0
  
  # mc study.
  rep = 1000
  
  for (r in 1:rep) {
    i = sample(1:n, n, replace = TRUE)
    skew = sample.skewness(xs[i])
    for (y in 1:nrow(ci.data)) {
      lower = ci.data[y,names[1]]
      upper = ci.data[y,names[2]]
      if (skew < lower) {
        ci.data[y,'left.miss'] = ci.data[y,'left.miss'] + 1
      } else if (skew > upper) {
        ci.data[y,'right.miss'] = ci.data[y,'right.miss'] + 1
      }
    }
  }
  
  ci.data$left.miss = ci.data$left.miss/rep
  ci.data$right.miss = ci.data$right.miss/rep
  
  return(ci.data)
}

mean = 3
sd = 4
xs = rnorm(n, mean = mean, sd = sd)

mc.skewness(xs)

df = 10
xs = rchisq(n, df = df)

mc.skewness(xs)