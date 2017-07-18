library("bootstrap")

B = 200
n = nrow(law)

theta.hat = cor(law$LSAT, law$GPA)
theta.hats.b = numeric(B)

ts = numeric(B)

for (b in 1:B) {
  i = sample(x = 1:n, size = n, replace = TRUE)
  law.b = law[i,]
  theta.hats.b[b] = cor(law.b$LSAT, law.b$GPA)
  sd.theta.hats.b = numeric(B)
  
  for(b2 in 1:B) {
    i2 = sample(x = 1:n, size = n, replace = TRUE)
    law.b2 = law.b[i2,]
    sd.theta.hats.b[b2] = cor(law.b2$LSAT, law.b2$GPA)
  }
  
  se.b = sd(sd.theta.hats.b)
  
  ts[b] = (theta.hats.b[b] - theta.hat) / se.b
}

alpha = 0.05
ts.ordered = sort(ts)

qs = quantile(ts.ordered, probs = c(alpha/2, 1-alpha/2))

se.hat = sd(theta.hats.b)

(CI = c(theta.hat - qs[2]*se.hat, theta.hat - qs[1]*se.hat))

hist(ts, breaks = 100, xlim = c(-5, 10))