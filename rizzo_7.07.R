library(bootstrap)


par(mfrow=c(5,1))
breaks = 100
hist(scor$mec, breaks = breaks)
hist(scor$vec, breaks = breaks)
hist(scor$alg, breaks = breaks)
hist(scor$ana, breaks = breaks)
hist(scor$sta, breaks = breaks)