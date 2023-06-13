# 1. Generate sample from Exponential distribution
set.seed(3225) # Fix the seed
lambda <- 27
N <- 1820
amostra <- rexp(N, rate = 1 / lambda) # Sampling

# 2. Calculate the occurrence times of events
sn <- rep(0, N) # Initialize a vector with 1820 positions filled with zeros
sn[1] <- amostra[1]
for (i in 2:N) {
    sn[i] <- sn[i - 1] + amostra[i]
}
T <- ceiling(sn[N])

# 3. Count the number of occurrences in each unit interval
subT <- rep(0, T)
for (i in 1:N) {
    subT[trunc(sn[i])] <- subT[trunc(sn[i])] + 1
}

# 4. Calculate average and absolute deviation
media <- sum(subT) / T
vesperado <- 1 / lambda
desvio_abs <- abs(media - vesperado)
round(desvio_abs, digits = 4)
