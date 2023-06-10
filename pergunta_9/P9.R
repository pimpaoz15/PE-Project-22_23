library(ggplot2)

# Set the seed for reproducibility
set.seed(1158)

# Define the sample sizes
n_values <- c(30, 50, 100, 200, 300, 500, 1000)

# Constants
k <- 1500
gamma <- 0.98
prob <- 0.7

# Function to calculate confidence interval length using method 1
calculate_ci_length_method1 <- function(n, z) {
  a <- 1
  b <- -2 * prob
  c <- prob^2 - z^2 * (prob * (1 - prob)) / n
  discriminant <- b^2 - 4 * a * c
  p1 <- (-b + sqrt(discriminant)) / (2 * a)
  p2 <- (-b - sqrt(discriminant)) / (2 * a)
  ci_length <- abs(p1 - p2)
  return(ci_length)
}

# Function to calculate confidence interval length using method 2
calculate_ci_length_method2 <- function(x_bar, n) {
  ci_length <- (x_bar - prob) / sqrt((x_bar * (1 - x_bar)) / n)
  return(ci_length)
}

# Calculate difference in means for each sample size
diff_means <- sapply(n_values, function(n) {
  z <- qnorm((1 + gamma) / 2)
  samples <- matrix(rbinom(k * n, size = 1, prob = prob), nrow = k)
  x_bars <- colMeans(samples)
  ci_lengths_method1 <- calculate_ci_length_method1(n, z)
  ci_lengths_method2 <- calculate_ci_length_method2(x_bars, n)
  mean_diff <- mean(ci_lengths_method1 - ci_lengths_method2)
  return(mean_diff)
})

# Create data frame
data <- data.frame(n = n_values, diff_means = diff_means)

# Save plot to a PDF file
pdf(file = "/Users/pimpao/Library/CloudStorage/OneDrive-Personal/Code/R/Projeto PE 22_23/pergunta_9/Rplot.pdf", width = 10, height = 10)

# Generate the plot
ggplot(data, aes(x = n, y = diff_means)) +
  geom_line() +
  geom_point() +
  labs(x = "Dimensão da amostra", y = "Diferença entre intervalos de confiança") +
  ggtitle("Diferença das médias dos intervalos de confiança da distribuição de Bernoulli") +
  theme_minimal()

# Close the PDF device
dev.off()
