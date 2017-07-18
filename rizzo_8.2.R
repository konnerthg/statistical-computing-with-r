soybean = chickwts$weight[chickwts$feed=="soybean"]
linseed = chickwts$weight[chickwts$feed=="linseed"]
n = length(soybean)
m = length(linseed)

tmp = min(n, m)
soybean = sort(soybean[1:tmp])
linseed = sort(linseed[1:tmp])

zs = c(soybean, linseed)
spearman.cor.test = cor.test(x = soybean, y = linseed, method = "spearman")

B = 1000
k = length(zs)

rhos = numeric(rep)

for (b in 1:B) {
  i = sample(1:k, k/2, replace = FALSE)
  xs = zs[i]
  ys = zs[-i]
  rhos[b] = cor(x = xs, y = ys, method = "spearman")
}

hist(rhos, breaks = 100)

(theta.hat = spearman.cor.test$estimate)

spearman.cor.test$p.value

(p.hat = mean(abs(rhos) > abs(theta.hat)))

(alpha = 0.05)

# p.hat < alpha, thus H0 rejected.