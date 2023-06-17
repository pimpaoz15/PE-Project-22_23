# Library imports
library(ggplot2)

# Set the seed for reproducibility
set.seed(1544)

# Define the Parameters
n <- 157
u <- 3.4
v <- 4

# Generate Cauchy sample
amostra_cauchy <- rcauchy(n, location = 3.2, scale = 2)
amostra_ordenada_cauchy <- sort(amostra_cauchy)

# Generate Normal sample
quantis <- (1:n) / (n + 1)
caso_cauchy <- qcauchy(quantis, location = 3.2, scale = 2)
caso_cauchy <- quantile(amostra_ordenada_cauchy, quantis)
caso_normal <- qnorm(quantis, u, sqrt(v))
# Calculate quantiles

quantis <- (1:n) / (n + 1)

# Create data frames
dados_cauchy <- data.frame(Quantil = amostra_ordenada_cauchy, Valor = caso_cauchy)
dados_normal <- data.frame(Quantil = amostra_ordenada_cauchy, Valor = caso_normal)

# Save plot to a PDF file
ggplot() +
    geom_point(data = dados_cauchy, aes(x = Valor, y = Quantil, color = "Cauchy")) +
    geom_point(data = dados_normal, aes(x = Valor, y = Quantil, color = "Normal")) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    labs(
        y = "Quantis de Probabilidade", x = "Valores Ordenados",
        title = "Amostras de Cauchy e Normal"
    ) +
    theme_minimal() +
    guides(color = guide_legend(title = "Variáveis"))

ggsave(filename = "8.pdf", width = 10, height = 10)
