x = rnorm(100, 1, 0)
y = rnorm(100, 1, 0)
library(goftest)
cvm.test(x, y)

# need to load package: https://rdrr.io/cran/RVAideMemoire/man/CvM.test.html
# won't work atm.