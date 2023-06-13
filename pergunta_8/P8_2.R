library(ggplot2)

# Fixar a semente para reprodutibilidade
set.seed(1544)

# Parâmetros da distribuição de Cauchy
location <- 3.2
scale <- 2

# Tamanho da amostra
n <- 157

# Gerar amostra de Cauchy
cauchy_sample <- rcauchy(n, location = location, scale = scale)

# Ordenar os valores gerados
sorted_cauchy_sample <- sort(cauchy_sample)

# Calcular os quantis
quantiles <- (1:n) / (n + 1)

# Criar um data frame com os valores de Cauchy ordenados e os quantis
cauchy_data <- data.frame(Quantile = quantiles, Value = sorted_cauchy_sample)

# Parâmetros da distribuição normal
mean_normal <- 3.4
sd_normal <- 4

# Gerar amostra da distribuição normal
normal_sample <- rnorm(n, mean = mean_normal, sd = sd_normal)

# Ordenar os valores gerados da distribuição normal
sorted_normal_sample <- sort(normal_sample)

# Criar um data frame com os valores da distribuição normal ordenados e os quantis
normal_data <- data.frame(Quantile = quantiles, Value = sorted_normal_sample)

# Criar um data frame com os quantis ímpares e a bissectriz
odd_quantiles <- quantiles[seq(1, n, by = 2)]
bisector <- odd_quantiles

# Criar um data frame para a bissectriz
bisector_data <- data.frame(Quantile = odd_quantiles, Value = bisector)

# Criar o gráfico
ggplot() +
    geom_line(data = cauchy_data, aes(x = Quantile, y = Value, color = "Cauchy"), linetype = "solid") +
    geom_line(data = normal_data, aes(x = Quantile, y = Value, color = "Normal"), linetype = "solid") +
    geom_line(data = bisector_data, aes(x = Quantile, y = Value), linetype = "dashed") +
    labs(
        x = "Quantis de probabilidade", y = "Valores gerados ordenados",
        title = "Amostras de Cauchy e Normal com Bissectriz dos Quadrantes Ímpares"
    ) +
    theme_minimal() +
    scale_color_manual(values = c("Cauchy" = "red", "Normal" = "blue")) +
    guides(color = guide_legend(title = "Distribuições"))

# Salvar o gráfico em um arquivo PDF
ggsave(filename = "cauchy_normal_plot.pdf", width = 10, height = 10)
