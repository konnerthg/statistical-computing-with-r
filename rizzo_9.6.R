sizes = c(125, 18, 20, 34)
size = sum(sizes)

m = 10000
burn = 2000
is = (burn+1):m

prob.vector = function (theta) {
  return(c(2 + theta, (1-theta), (1-theta), theta) / 4)
}

prob.ratio = function (n, d) {
  #print(prob.vector(n)^sizes)
  #print(prob.vector(d)^sizes)
  return(prod(prob.vector(n)^sizes / prob.vector(d)^sizes))
}

# random walk metropolis.
# using unif(-0.25, 0.25) as step.

x.rw = numeric(m)
k.rw = 0
u = runif(m)
v = runif(m, -0.25, 0.25)
x.rw[1] = v[1]
for (i in 2:m) {
  xt = x.rw[i-1]
  y = xt + v[i]
  r = min(prob.ratio(y, xt), 1)
  #print('hippedy')
  #print(xt)
  #print(y)
  #print(r)
  if (!is.nan(r) && u[i] <= r) {
    x.rw[i] = y
  } else {
    k.rw = k.rw + 1
    x.rw[i] = xt
  }
}

# metropolis hastings

sd = 0.5
min = -0.8
max = 0.8

rg = function(p) {
  return(runif(1, min - abs(p), max + abs(p)))
  # return(rnorm(1, p, sd))
}

dg = function(x, p) {
  return(dunif(x, min - abs(p), max + abs(p)))
  # return(dnorm(x = x, mean = p, sd = sd))
}

x.mh = numeric(m)
k.mh = 0
u = runif(m)
x.mh[1] = rg(0)
for(i in 2:m) {
  xt = x.mh[i-1]
  y = rg(xt)
  r = min(prob.ratio(y, xt) * dg(xt, y) / dg(y, xt), 1)
  if (!is.na(r) && u[i] <= r) {
    x.mh[i] = y
  } else {
    x.mh[i] = xt
    k.mh = k.mh + 1
  }
}

# independence sampler.

x.i = numeric(m)
x.i[1] = rg(0)
k.i = 0
u = runif(m)
for (i in 2:m){
  xt = x.i[i-1]
  y = rg(0)
  r = prob.ratio(y, xt) * dg(xt, 0)/dg(y, 0)
  if (u[i] <= r) {
    x.i[i] = y
  } else {
    x.i[i] = xt
    k.i = k.i + 1
  }
}

print(k.rw)
print(k.mh)
print(k.i)

par(mfrow = c(3,2))

xs = as.list(x.rw, x.mh)

x = x.rw[is]
hist(x, probability = TRUE)
plot(is, x, type='l')

x = x.mh[is]
hist(x, probability = TRUE)
plot(is, x, type='l')

x = x.i[is]
hist(x, probability = TRUE)
plot(is, x, type='l')

par(mfrow = c(1,1))