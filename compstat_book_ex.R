#ex 3.1

two.exp = function (size, lambda, eta) {
  r = runif(size)
  -1/lambda * log(u / (-exp(lambda * eta)) + 1)
}

n = 10000
lambda = 3
eta = 0
a = rexp(1000, rate = lambda)
b = two.exp (n, lambda, eta)

p1 = qplot(a, xlim = c(0,4), binwidth = 0.05)
p2 = qplot(b, xlim = c(0,4), binwidth = 0.05)

grid.arrange(p1, p2)

round(x = rbind(quantile(a, probs = seq(0, 1, 0.05)), quantile(b, probs = seq(0, 1, 0.05))), digits = 3)

# ex 3.2

library(ggplot2)
library(gridExtra)
library(reshape)

laplace = function (size) {
  u = runif(size)
  sapply(u, function (v) if (v > 0.5) { -log(-2*v + 2) } else { log(2*v) })
}
n = 1000
range = c(-10,10)
step = 0.05

truth = function (v) {
  1/2*exp(-abs(v))
}

x = seq(range[1], range[2], length.out = n)
xy = truth(x)
y = laplace(1000)

#lines(density(xy))

m = cbind(x, xy)
colnames(m) = c("x", "f(x)")

# qplot(y/n, xlim = range, geom="histogram", binwidth = step) + stat_function(fun = truth)
hist(y, probability = TRUE, breaks = 100, xlim = range)
lines(x, y = xy)

# ex 3.3

rpareto = function (size, a, b) {
  stopifnot(a > 0 && b > 0)
  u = runif(size)
  b*(1-u)^(-1/a)
}

dpareto = function (x, a, b) {
  stopifnot(a > 0 && b > 0)
  sapply(x, function (v) if (v < b) { 0 } else { a*b^a*v^(-a-1) })
}

size = 1000
a = 2
b = 2
sam = rpareto(1000, a, b)

hist(sam, probability = TRUE, breaks = 100)
ran = range(sam)
x = seq(ran[1], ran[2], 0.01)
y = dpareto(x, a, b)
lines(x = x, y = y)

# ex 3.4

rrayleigh = function (size, sigma) {
  stopifnot(sigma > 0)
  u = runif(size)
  sqrt(-2*sigma^2*log(1 - u))
}

drayleigh = function (x, sigma) {
  stopifnot(sigma > 0)
  y = x / (sigma^2) * exp(- x^2 / (2 * sigma^2))
  y[x < 0] = 0
  y
}

size = 1000
library(ggplot2)
library(gridExtra)

sigmas = 1:5
xs = sapply(sigmas, function (v) rrayleigh(size, v))
mat = matrix(xs, ncol = length(sigmas), dimnames = list(NULL, sigmas))
df = data.frame(mat)
ps = lapply(1:ncol(df), function (v) qplot(x = df[,v])) 
p1 = qplot(df$X1, main = paste(c("Sigma = ", sigmas[1]), collapse = " "), binwidth = 0.1) + scale_x_continuous(breaks = c(1:range(df$X1)[2]))
p2 = qplot(df$X2, main = paste(c("Sigma = ", sigmas[2]), collapse = " "), binwidth = 0.1) + scale_x_continuous(breaks = c(1:range(df$X2)[2]))
p3 = qplot(df$X3, main = paste(c("Sigma = ", sigmas[3]), collapse = " "), binwidth = 0.1) + scale_x_continuous(breaks = c(1:range(df$X3)[2]))
p4 = qplot(df$X4, main = paste(c("Sigma = ", sigmas[4]), collapse = " "), binwidth = 0.1) + scale_x_continuous(breaks = c(1:range(df$X4)[2]))
p5 = qplot(df$X5, main = paste(c("Sigma = ", sigmas[5]), collapse = " "), binwidth = 0.1) + scale_x_continuous(breaks = c(1:range(df$X5)[2]))

grid.arrange(p1, p2, p3, p4, p5)

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

sapply(df, mode)

# ex 3.5

xs = 0:4
ps = c(0.1, 0.2, 0.2, 0.2, 0.3)
size = 100000

rdisc = function (size, xs, ps) {
  u = runif(size)
  c = cumsum(ps)
  sapply(u, function(v) min(xs[c >= v]))
}

sam = rdisc(size, xs, ps)
r1 = table(sam)
truth = sample(size = size, x = xs, prob = ps, replace = TRUE)
r2 = table(truth)
# hist(sam, probability = TRUE)

rel.freq = rbind(r1, r2)/size

# ex 3.7

general.acc.rej = function (size, xs, df, dgen, rgen, c) {
  acc.rej = function (size, df, rgen, dgen, c) {
    ct = 1
    n = 1
    res = numeric(size)
    while(n <= size) {
      y = rgen(1)
      u = runif(1)
      if (u < df(y)/dgen(y)/c) {
        res[n] = y
        n = n + 1
      }
      ct = ct + 1
    }
    #print((n-1)/(ct-1))
    res
  }
  
  
  sam = acc.rej(size = size, df = df, rgen = rgen, dgen = dgen, c)
  
  #ys = df(xs)
  
  #nrBins = 50
  # Compute the maximum value in the histogram for a better visualization.
  #bins = seq(min(xs), max(xs), by = dist(range(xs))/nrBins)
  #hist.vals = table(cut(x = sam, bins))
  # adapt to a total area of 1 (probability histogram).
  #hist.vals = hist.vals/sum(hist.vals) * nrBins
  # y limits as maximum of distribution and sample.
  #ylim = c(min(c(ys, hist.vals)), max(c(ys, hist.vals)))
  
  #hist(sam, probability = TRUE, ylim = ylim, breaks = nrBins)
  #lines(x = xs, y = ys)
  
  sam
}

ex3.7 = function(size, a, b) {
  rgen = function (size) {
    runif(size, min = u.min, max = u.max)
  }
  
  dgen = function (x) {
    dunif(x, min = u.min, max = u.max)
  }
  
  df = function (x) {
    dbeta(x = x, shape1 = a, shape2 = b)
  }
  
  xs = seq(0, 1, 0.01)
  u.max = max(xs)
  u.min = min(xs)
  y.max = max(df(xs))
  c = y.max / (u.max - u.min)
  
  general.acc.rej (size = size, xs = xs, df = df, dgen = dgen, rgen = rgen, c = c)
  
}

a = 3
b = 2
size = 1000

ex3.7 (size = size, a = a, b = b)

# ex 3.8

library(ggplot2)
library(gridExtra)

lognormal = function (size, mu, sigma) {
  exp(rnorm(n = size, mean = mu, sd = sigma))
}

mus = c(0, 0, 0, 1, 2, 3)
sigmas = c(0.25, 0.5, 1, 0.25, 0.5, 1)
size = 1000

params = sapply(1:length(mus), function (v) c(mus[v], sigmas[v]))
params = split(params, rep(1:ncol(params), each = nrow(params)))

sams = data.frame(lapply(params, function (v) lognormal(size = size, mu = v[1], sigma = v[2])))
truths = data.frame(lapply(params, function (v) rlnorm(n = size, meanlog = v[1], sdlog = v[2])))
colnames(sams) = params
colnames(truths) = params

lims = lapply(1:length(params), function (v) c(min(c(sams[,v], truths[,v])), max(c(sams[,v], truths[,v]))))

ps.truth = lapply(1:ncol(truths), function (v) ggplot(data = truths, aes(x = truths[,v])) + geom_density() + ggtitle(paste("truth, mean =", params[[v]][1], ", sd =", params[[v]][2], sep = " ")) + scale_x_continuous(limits = lims[[v]]))
ps.sams = lapply(1:ncol(sams), function (v) ggplot(data = sams, aes(x = sams[,v])) + geom_density() + ggtitle(paste("sample, mean =", params[[v]][1], ", sd =", params[[v]][2], sep = " ")) + scale_x_continuous(limits = lims[[v]]))

do.call("grid.arrange", c(grobs = c(ps.truth, ps.sams), ncol = 2, as.table = FALSE))

# ex 3.9

depanechnikov = function (x) {
  3/4 * (1 - x^2)
}

repanechnikov = function (size) {
  u1 = runif(size, min = -1, max = 1)
  u2 = runif(size, min = -1, max = 1)
  u3 = runif(size, min = -1, max = 1)
  sapply(1:size, function(v)
    if (abs(u3[v]) >= abs(u2[v]) && abs(u3[v]) >= abs(u1[v])) u2[v] else u3[v]
  )
}

size = 1000
sam = repanechnikov(size = size)
xlim = range(sam)
xs = seq(from = xlim[1], to = xlim[2], by = 0.01)

hist(sam, probability = TRUE)
lines(x = xs, y = depanechnikov(xs))

# ex 3.11

means = c(0, 3)
sds = c(1, 1)
ps = seq(0.1, 0.9, 0.07)
size = 1000

rmix = function (size, p, means, sds) {
  rnorm(size, mean = sample(c(means[1], means[2], replace = TRUE, probs = c(p, 1-p))), sd = sample(c(sds[1], sds[2], replace = TRUE, probs = c(p, 1-p))))
}

sams = data.frame(lapply(ps, function (p) rmix(size = size, p = p, means = means, sds = sds)))
colnames(sams) = ps

plots = lapply(1:ncol(sams), function (v) ggplot(data = sams, aes(x = sams[,v])) + geom_density() + geom_histogram(aes(y = ..density..), bins = 100) + ggtitle(paste("p =", ps[v], sep = " ")))


do.call("grid.arrange", c(grobs = c(plots), ncol = 2, as.table = FALSE))

# hist(sam, probability = TRUE, ylim = c(0, 0.5))
# lines(density(sam))

# ex 3.12

size = 1000
r = 4
beta = 2

rmix = function (size, r, beta) {
  rexp(n = size, rgamma(n = size, r, beta))
}

sam = rmix(size = size, r = r, beta = beta)

hist (sam, probability = TRUE, breaks = 100, ylim = c(0, 2))
# ex 3.13
xs = seq(min(sam), max(sam), 0.01)
lines(x = xs, y = dpareto(xs, a = r, b = beta))

# ex 3.14

mus = c(0, 1, 2)
size = 2000
d = length(mus)
Sigma = matrix(c(1, -0.5, 0.5, -0.5, 2, -0.5, 0.5, -0.5, 3), ncol = 3)

Zs = matrix(rnorm(n = d * size), ncol = size, nrow = d)
C = t(chol(Sigma))

Ys = t(C %*% Zs + mus)

# pairs(Ys)

# ex 3.15

normalize = function (data) {
  Sigma = cov(data)
  mus = sapply(1:ncol(data), function (v) mean(data[,v]))
  A = chol(solve(Sigma))
  Xs = t(A %*% (t(data)-mus))
  Xs
}


# ex 3.16

library(bootstrap)
norm.scor = scor
m1 = normalize(data.matrix(norm.scor[,1:2]))
m2 = normalize(data.matrix(norm.scor[,3:5]))
norm.scor[,1:2] = m1
norm.scor[,3:5] = m2
cov = round(cov(norm.scor), digits=4)

# ex 3.17

iter = 30
size = 5000

a = 1
b = 5

tt1 = 0
tt2 = 0

for(i in 1:iter) {
  tt1 = tt1 + system.time(ex3.7(size = size, a = a, b = b))["elapsed"]
  tt2 = tt2 + system.time(rbeta(n = 1000, shape1 = a, shape2 = b))["elapsed"]
}

tt1
tt2

# ex 3.18

my.rwishart = function (Sigma, n) {
  stopifnot(nrow(Sigma) == ncol(Sigma))
  d = ncol(Sigma)
  L = chol(Sigma)
  tmp = matrix(rnorm(d^2), ncol = d, nrow = d)
  tmp[upper.tri(tmp, diag = TRUE)] = 0
  A = diag(sapply(1:d, function (v) rchisq(1, n - v + 1))) + tmp
  t(L) %*% A %*% t(A) %*% L
}

Sigma = matrix(c(1, -0.5, 0.5, -0.5, 2, -0.5, 0.5, -0.5, 3), ncol = 3)
n = 4
size = 1000

sam = lapply(1:size, function (v) my.rwishart(Sigma, n))

(mean = apply(simplify2array(sam), 1:2, mean))
sd = apply(simplify2array(sam), 1:2, sd)

(truth.mean = n * Sigma)