set.seed(1306)
m <- 150
n <- 33
alfa <- 0.1
c <- 1.645

total <- 0
samples <- replicate(m, {
    a <- rnorm(n, 58.4, 2)
})

for (i in 1:m) {
    b <- samples[1:n, i]
    z_ho <- (mean(b) - 57.4) / (2 / sqrt(n))

    if ((z_ho > (-c)) & (z_ho < c)) {
        total <- total + 1
    }
}
probability <- total / m

round(probability, 3)
