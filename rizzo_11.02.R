a = 10
b = 20
r = 5
s = 5

getP = function (a, b, r, s) sum(sapply(max(r-b, 0):(r-1), function(k) choose(r+s-1,k)*choose(a+b-1, a+r-1-k)/choose(a+b+r+s-2,a+r-1)))

P.numerical = getP(a, b, r, s)

n = 10000
xs = rbeta(n, shape1 = a, shape2 = b)
ys = rbeta(n, shape1 = r, shape2 = s)
P.mc = mean(xs < ys)

P.numerical
P.mc