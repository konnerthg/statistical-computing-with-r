# ex 3.19
p = 0.5
s.min = 0
s.max = 20
s.start = 10

random.walk = function (s) {
  u = runif(1)
  n = s[length(s)]
  if (u > p) {
    n = n -1
  } else {
    n = n + 1
  }
  if (n <= s.min || n >= s.max) {
    return(c(s, n))
  } else {
    return(random.walk(c(s,n))) 
  }
}

s = random.walk(s.start)
plot(y = s, x = 1:length(s), type="l")