# WIP: the description of this exercise makes no sense at all.
# 499 permutation replicates of what statistics?
# how can there be 499 replicates when there are 10000 samples?
# what is a permutation decision?

library(FastKNN)
library(pdist)
library(energy)
library(mvtnorm)
library(boot)

# returns T.
rth.nn = function (dist, xi, sizes, nr.neighbors) {
  n1 = sizes[1]
  n2 = sizes[2]
  n = n1 + n2
  
  perm.dist = dist[xi,]
  
  # matrix where row i represents k nearest neighbors of z[i]
  nn = matrix(0, n, nr.neighbors)
  for (i in 1:n) {
    nn[i,] = k.nearest.neighbors(i, perm.dist, k = nr.neighbors)
  }
  
  block1 = nn[1:n1,]
  block2 = nn[n1+1:n2,]
  
  T = (sum(block1<=n1) + sum(block2>n1))/(n*nr.neighbors)
  
  return (T)
}

n1 = 20
n2 = 20
d = 2
a = 2/sqrt(d)
sizes = c(n1, n2)
mean1 = c(0, 0)
mean2 = c(0, 0.5)
Sigma1 = diag(d)
Sigma2 = diag(d)
x = rmvnorm(n = n1, mean = mean1, sigma = Sigma1)
y = rmvnorm(n = n2, mean = mean2, sigma = Sigma2)
z = rbind(x, y)
dist = as.matrix(dist(z))
rep = 999
nr.neighbors = 3

e = eqdist.etest(dist, sizes, TRUE)

# boot.obj = boot(data = dist, statistic = eqdist.etest, sim = "permutation", R = rep, sizes = sizes, distance = TRUE)

boot.obj = boot(data = dist, statistic = rth.nn, sim = "permutation", R = rep, sizes = sizes, nr.neighbors = nr.neighbors)


if (FALSE){
total.n.x = dim(x)[1]
total.n.y = dim(y)[1]
total.sizes = c(total.n.x, total.n.y)
r = 3

T0 = rth.nn(dist, total.sizes, r)
e0 = eqdist.etest(dist, total.sizes, TRUE)$statistic

Ts = numeric(rep)
es = numeric(rep)

for (i in 1:rep) {
  k1 = sample(1:total.n.x, n1, replace = FALSE)
  k2 = total.n.x + sample(1:total.n.y, n2, replace = FALSE)
  ks = c(k1, k2)
  # shuffle distance matrix according to permutation.
  dist.i = dist[ks,ks]
  Ts[i] = rth.nn(dist.i, sizes, r)
  es[i] = eqdist.etest(dist.i, sizes, TRUE)$statistic
}

par(mfrow=c(1,2))
hist(Ts)
hist(es)

(p.e = mean(es >= e0))
(p.T = mean(Ts >= T0))
}