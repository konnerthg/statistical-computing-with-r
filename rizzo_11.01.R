# useful example value of x.
x = 77.34599 #runif(1, 1, 100)
a = log(exp(x))
b = exp(log(x))

x == a
x == b
a == b

identical(x, a)
identical(x, b)
identical(b, a)

all.equal(x, a)
all.equal(x, b)
all.equal(b, a)
