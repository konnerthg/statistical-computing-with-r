mu = 4
sigma = 3
n = 1000

xs = rnorm(n, mean = mu, sd = sqrt(sigma))

# sample mean.
mu.hat = mean(xs)

# compute bootstrap sample of the mean.
B = 200
mu.hats.b = numeric(B)
ts = numeric(B)

for (b in 1:B) {
  i = sample(1:n, n, replace = TRUE)
  xs.b = xs[i]
  mu.hats.b[b] = mean(xs.b)
  
  for (b2 in 1:B) {
    i2 = sample(1:n, n, replace = TRUE)
    ts[b] = (mu.hats.b[b] - mu.hat) / sd(xs.b[i])
  }
}

se.hat = sd(mu.hats.b)

# visualize.
par(mfrow = c(2,1))
hist(mu.hats.b, breaks = 100)
hist(ts, breaks = 100)

# compute CIs.
alpha = 0.05
probs = c(alpha/2, 1-alpha/2)

names = sapply(probs, function(p) paste(p*100, '%', sep = ''))
setCINames = function (object) {
  return (setNames(object = object, names))
}

# standard normal.
qs.norm = qnorm(probs)
ci.sn = setCINames(mu.hat - rev(qs.norm)*se.hat)

# basic bootstrap.
qs.mu.hats.b = quantile(x = mu.hats.b, probs = probs)
ci.basic = setCINames(2*mu.hat - rev(qs.mu.hats.b))

# percentile.
ci.percentile = setCINames(quantile(mu.hats.b, probs = probs))

# bootstrap t.
qs.ts = quantile(ts, probs = probs)
ci.t = setCINames(mu.hat - rev(qs.ts)*se.hat)

# set up data for the MC study.
mc.study = data.frame(rbind(ci.sn, ci.basic, ci.percentile, ci.t))
colnames(mc.study) = names
mc.study['miss.left'] = rep.int(0, times = nrow(mc.study))
mc.study['miss.right'] = rep.int(0, times = nrow(mc.study))

# compute coverage rates for sample mean when sampling from the normal population xs.
size = n
rep = 10000
miss.l = 0
miss.r = 0

for(r in 1:rep) {
  i = sample(1:n, size, replace = TRUE)
  mu.sample = mean (xs[i])
  for(y in 1:nrow(mc.study)) {
    lower = mc.study[y,names[1]]
    upper = mc.study[y,names[2]]
    if (mu.sample < lower) {
      mc.study[y,'miss.left'] = mc.study[y,'miss.left'] + 1
    } else if (mu.sample > upper) {
      mc.study[y,'miss.right'] = mc.study[y,'miss.right'] + 1
    }
  }
}

mc.study$miss.left = mc.study$miss.left/rep
mc.study$miss.right = mc.study$miss.right/rep

mc.study