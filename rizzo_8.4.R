# the package from the book doesn't seem to exist.
library(FastKNN)
library(pdist)

# returns T.
rth.nn = function (x, y, r) {
  n1 = length(x)
  n2 = length(y)
  
  z = matrix(c(x,y), ncol = 1)
  n = dim(z)[1]
  
  # distance matrix between z and z.
  distance_matrix = as.matrix(dist(z))
  
  # matrix where row i represents k nearest neighbors of z[i]
  nn = matrix(0, n, r)
  for (i in 1:n) {
    nn[i,] = k.nearest.neighbors(i, distance_matrix, k = r)
  }

  block1 = nn[1:n1,]
  block2 = nn[n1+1:n2,]
  
  T = (sum(block1<=n1) + sum(block2>n1))/(n*k)
  
  return (T)
}

feed1 = "sunflower"
feed2 = "linseed"
x = chickwts$weight[chickwts$feed == feed1]
y = chickwts$weight[chickwts$feed == feed2]
r = 3

rth.nn(x, y, r)