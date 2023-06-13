set.seed(131)
m <- 300
n <- 45
alfa <- 0.02
c <- 2.326

total <- 0
samples <- replicate(m, {
    a <- rnorm(n, 38.7, 2)
})

for (i in 1:m) {
    b <- samples[1:n, i]
    z_ho <- (mean(b) - 37.5) / (2 / sqrt(n))

    if ((z_ho > (-c)) && (z_ho < c)) {
        total <- total + 1
    }
}
probability <- total / m
print(probability)
