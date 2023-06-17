set.seed(3225)
k <- 1820
lambda <- 27
x <- rexp(k, lambda)
s <- cumsum(x)
T <- ceiling(s[k])
counts <- rep(0, T)
for (i in 1:k) {
    counts[ceiling(s[i])] <- counts[ceiling(s[i])] + 1
}
mean_counts <- mean(counts)
expected_counts <- lambda
abs_deviation <- abs(mean_counts - expected_counts)
round(abs_deviation, digits = 4)
