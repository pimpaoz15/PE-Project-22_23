# set seed
set.seed(1275)

# generate sample
amostra <- rexp(2493, rate = 12.5)

# set parameters
sj <- cumsum(amostra)
T <- ceiling(sj[2493])

# set unitary subintervals
subintervalos <- seq(1, T, by = 1)
contagem <- table(cut(sj, breaks = subintervalos, right = FALSE)) # right = FALSE argument specifies that the intervals should be left-closed and right-open.
media_obs <- mean(contagem)
desvio_absoluto <- abs(media_obs - 12.5)
desvio_absoluto <- round(desvio_absoluto, 4)

# print result
print(desvio_absoluto)
