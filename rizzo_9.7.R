m = 5000
burn = 1000

x = matrix(0, m, 2)

rho = 0.9
mu1 = 0
mu2 = 0
sigma1 = 1
sigma2 = 1
s1 = sqrt(1-rho^2)*sigma1
s2 = sqrt(1-rho^2)*sigma2

mean12 = function (x2) mu1 + rho*sigma1/sigma2*(x2 - mu2)
mean21 = function (x1) mu2 + rho*sigma2/sigma1*(x1 - mu1)

x[1,] = c(mu1, mu2)

for (i in 2:m) {
  xt = x[i-1,]
  xt[1] = rnorm(1, mean12(xt[2]), s1)
  xt[2] = rnorm(1, mean21(xt[1]), s2)
  x[i,] = xt
}

x = x[(burn+1):m,]

x = data.frame(x)
lin.reg = lm(X1 ~ X2, data = x)

par(mfrow=c(1,2))
plot(x, cex = 0.5, main = "generated data")
hist(lin.reg$residuals, main = "residuals of linear model")
par(mfrow=c(1,1))