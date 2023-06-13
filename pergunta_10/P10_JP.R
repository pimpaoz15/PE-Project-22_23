set.seed(615)
m <- 100
n <- 31
alfa <- 0.1
c <- 1.645

total <- 0
samples <- replicate(m, {
    a <- rnorm(n, 72.6, 2)
})

for (i in 1:m) {
    b <- samples[1:n, i]
    z_ho <- (mean(b) - 71.4) / (2 / sqrt(n))

    if ((z_ho > (-c)) && (z_ho < c)) {
        total <- total + 1
    }
}
probability <- total / m
print(probability)
