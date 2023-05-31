# Set the seed for reproducibility
set.seed(1544)

# Define the sample size
sample_size <- 157

# Generate Cauchy sample
cauchy_sample <- rcauchy(sample_size, location = 3.2, scale = 2)
sorted_cauchy_sample <- sort(cauchy_sample)

# Generate Normal sample
normal_sample <- rnorm(sample_size, mean = 3.4, sd = 4)
sorted_normal_sample <- sort(normal_sample)

# Calculate quantiles
quantiles <- (1:sample_size) / (sample_size + 1)

# Create data frames
cauchy_data <- data.frame(Quantile = quantiles, Value = sorted_cauchy_sample)
normal_data <- data.frame(Quantile = quantiles, Value = sorted_normal_sample)

# Save plot to a PDF file
pdf(file = '/Users/pimpao/Library/CloudStorage/OneDrive-Personal/Code/R/Projeto PE 22_23/pergunta_8/Rplot.pdf', width = 10, height = 10)

# Generate the plot
ggplot() +
  geom_line(data = cauchy_data, aes(x = Quantile, y = Value, color = "Cauchy"), linetype = "solid") +
  geom_line(data = normal_data, aes(x = Quantile, y = Value, color = "Normal"), linetype = "solid") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(x = "Quantiles de Probabilidade", y = "Valores Ordenados", 
       title = "Amostras de Cauchy e Normal") +
  theme_minimal() +
  guides(color = guide_legend(title = "Variáveis"))

# Close the PDF device
dev.off()
