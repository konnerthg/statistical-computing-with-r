library(bootstrap)

pairs(scor)
cor(scor)

n = nrow(scor)
B = 200

ro.12 = numeric(B)
ro.34 = numeric(B)
ro.35 = numeric(B)
ro.45 = numeric(B)

for (b in 1:B) {
  i = sample(1:n, n, replace = TRUE)
  scor.b = scor[i,]
  ro.12[b] = cor(scor.b$mec, scor.b$vec)
  ro.34[b] = cor(scor.b$alg, scor.b$ana)
  ro.35[b] = cor(scor.b$alg, scor.b$sta)
  ro.45[b] = cor(scor.b$ana, scor.b$sta)
}

(se.12 = sd(ro.12))
(se.34 = sd(ro.34))
(se.35 = sd(ro.35))
(se.45 = sd(ro.45))