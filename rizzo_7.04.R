library(boot)
hours = aircondit$hours
n = length(hours)

# MLE yeilds:
mle.lambda = function (values) {
  return(length(values)/sum(values))
}

lambda.hat = mle.lambda(hours)

lambda.hats.b = numeric(B)

B = 200
for (b in 1:B) {
  i = sample(1:n, n, replace = TRUE)
  hours.b = hours[i]
  lambda.hats.b[b] = mle.lambda(hours.b)
}

lambda.hats.b.mean = mean(lambda.hats.b)

(bias = lambda.hats.b.mean - lambda.hat)