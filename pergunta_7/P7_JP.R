set.seed(1114)
m <- matrix(NA, 1248, 10, TRUE)
for (i in 1:1248) {
    for (j in 1:10) {
        m[i, j] <- rnorm(1, 0, 1)
    }
}


m2 <- m * m
m3 <- rowSums(m2)
q056 <- quantile(m3, 0.47, type = 2)
Resultado <- round(q056, digits = 4)
print(Resultado)
