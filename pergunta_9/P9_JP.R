# Setting the seed for reproducibility
set.seed(1158)

# Initializing Parameters
k <- 1500
gamma <- 0.98
prob <- 0.7

# Initializing variables
n <- c(30, 50, 100, 200, 300, 500, 1000)
z <- qnorm((1 + gamma) / 2)
a <- qnorm((1 - gamma) / 2 + gamma)

# Creating empty vectors
comp_1 <- numeric()
comp_2 <- numeric()
dif_mean <- numeric()

# Loop over different values of n
for (i in 1:length(n)) {
    # Generating samples
    samples <- replicate(k, {
        a <- rbinom(n[i], 1, prob)
    })
    s_mean <- colMeans(samples)
    for (j in 1:length(s_mean)) {
        solutions <- Re(polyroot(c(s_mean[j]^2, (-2 * n[i] * s_mean[j] - z^2) / n[i], (n[i] + z^2) / n[i])))
        comp_1 <- c(comp_1, solutions[2] - solutions[1])
        comp_2 <- c(comp_2, (s_mean[j] + a * sqrt((s_mean[j] * (1 - s_mean[j])) / n[i])) - (s_mean[j] - a * sqrt((s_mean[j] * (1 - s_mean[j])) / n[i])))
    }
    dif_atual <- abs(comp_2 - comp_1)
    dif_mean <- c(dif_mean, mean(dif_atual))

    # Resetting variables for next iteration
    comp_1 <- 0
    comp_2 <- 0
    dif_atual <- 0
}

# Initialise the directory to save the plot
pdf(file = "/Users/pimpao/Library/CloudStorage/OneDrive-Personal/Code/R/Projeto PE 22_23/pergunta_9/Rplot.pdf", width = 10, height = 10)

# Plotting the results with connected lines
plot(n, dif_mean, type = "b", pch = 16, xlab = "n", ylab = "Diferença mediana", main = "Diferença mediana para k = 1500 Vs n")
lines(n, dif_mean, type = "l", col = "red", lwd = 2)

dev.off()
