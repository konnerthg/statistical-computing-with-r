attach(chickwts)
feed1 = "soybean"
feed2 = "linseed"
xs = sort(weight[feed == feed1])
ys = sort(weight[feed == feed2])
rep = 1000

library(RVAideMemoire)

zs = c(xs, ys)
n1 = length(xs)
n2 = length(ys)
n = n1 + n2

Ts = numeric(rep)
for (i in 1:rep) {
  ks = sample(1:n, n1, replace = FALSE)
  zs1 = zs[ks]
  zs2 = zs[-ks]
  Ts[i] = CvM.test(zs1, zs2)$statistic
}

(cvm = CvM.test(x, y))
T.hat = cvm$statistic
(p.hat = mean(abs(T.hat) < abs(Ts)))

hist(Ts)