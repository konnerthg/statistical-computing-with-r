library(boot)
hours = aircondit$hours
n = length(hours)

# MLE yeilds:
mle.lambda = function (values) {
  return(length(values)/sum(values))
}

time.b = numeric(B)
ts = numeric(B)

time.hat = 1/mle.lambda(hours)

B = 200
for (b in 1:B) {
  i = sample(1:n, n, replace = TRUE)
  hours.b = hours[i]
  time.b[b] = 1/mle.lambda(hours.b)
  
  times.b2 = numeric(B)
  
  # compute bootstrap ts for later use.
  for (b2 in 1:B) {
    i2 = sample(1:n, n, replace = TRUE)
    hours.b2 = hours.b[i2]
    times.b2[b2] = 1/mle.lambda(hours.b2)
  }
  
  ts[b] = (time.b[b] - time.hat) / sd(times.b2)
}

se.hat = sd(time.b)
alpha = 0.05;
q.probs = c(alpha/2, 1-alpha/2)

setCINames = function (object) {
  return(setNames(object, c(paste((alpha/2)*100, '%'), paste((1-alpha/2)*100, '%'))))
}

# plot observed statistic 1/lambda, as well as t values for comparison with the bootstrap t CI.
par(mfrow=c(1,2))
hist(time.b, breaks = 100)
hist(ts, breaks = 100)

# standard normal.
q = qnorm(1-alpha/2)
ci.sn = time.hat + c(-1,1)*q*se.hat
(ci.sn = setCINames(ci.sn))

# basic boostrap.
qs.time.hat = quantile(x = time.b, p = q.probs)
ci.basic = rev(2*time.hat - qs.time.hat)
(ci.basic = setCINames (object = ci.basic))

# percentile.
(ci.percentile = qs.time.hat)

# bootstrap t.
qs.t = quantile(x = ts, p = q.probs)
(ci.t = setCINames(rev(time.hat - qs.t*se.hat)))