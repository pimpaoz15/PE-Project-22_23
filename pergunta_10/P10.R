# Resposta Excel ->

####################################################################################################
# Set the seed to 615
set.seed(615)

# Generate 100 samples of size 31 from a normal distribution
m <- 100
n <- 31
mu <- 72.6
sigma_sq <- 4

samples <- matrix(rnorm(m * n, mean = mu, sd = sqrt(sigma_sq)), nrow = n)

# Perform t-test for each sample
null_value <- 71.4
alpha <- 0.1

results <- vector("list", m)

for (i in 1:m) {
    t_stat <- t.test(samples[, i], mu = null_value)$statistic
    p_value <- t.test(samples[, i], mu = null_value)$p.value

    result <- ifelse(p_value <= alpha, "Reject H0", "Fail to reject H0")
    results[[i]] <- result
}

# Calculate the estimated probability of not rejecting H0
num_non_reject <- sum(sapply(results, function(x) x == "Fail to reject H0"))
estimated_prob <- num_non_reject / m

# Print the estimated probability with 5 decimal digits
cat("Estimated probability of not rejecting H0:", sprintf("%.3f", estimated_prob), "\n")
