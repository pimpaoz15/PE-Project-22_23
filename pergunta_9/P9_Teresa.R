rm(list = ls())

library(ggplot2)

set.seed(1158) # Fixar a semente para reprodutibilidade

n_values <- c(30, 50, 100, 200, 300, 500, 1000) # Valores de n

gamma <- 0.98 # Nível de confiança aproximado

k <- 1500 # Número de amostras

differences <- vector("numeric", length(n_values)) # Vetor para armazenar as diferenças médias

for (i in 1:length(n_values)) {
    n <- n_values[i]
    p <- 0.7

    interval_diffs <- vector("numeric", k) # Vetor para armazenar as diferenças dos intervalos

    for (j in 1:k) {
        # Gerar amostra de uma distribuição de Bernoulli
        sample <- rbinom(n, 1, p)

        # Calcular as estatísticas necessárias para o Método 1
        X_bar <- mean(sample)
        z <- qnorm(1 - gamma / 2)
        a <- X_bar^2 - 2 * p * X_bar + p^2 - z^2 * p * (1 - p) / n

        # Calcular as estatísticas necessárias para o Método 2
        Z2 <- (X_bar - p) / sqrt(X_bar * (1 - X_bar) / n)

        # Calcular os intervalos de confiança
        interval_1 <- polyroot(c(a, -2 * X_bar, 1))
        interval_2 <- c(X_bar - z * sqrt(p * (1 - p) / n), X_bar + z * sqrt(p * (1 - p) / n))

        # Calcular a diferença dos comprimentos dos intervalos
        diff_length <- abs(interval_2[2] - interval_2[1]) - abs(interval_1[2] - interval_1[1])

        interval_diffs[j] <- diff_length
    }

    # Calcular a média das diferenças
    differences[i] <- mean(interval_diffs)
}

# Criar o gráfico
data <- data.frame(n = n_values, differences = differences)

plot <- ggplot(data, aes(x = n, y = differences)) +
    geom_line(color = "blue") +
    labs(x = "Tamanho da Amostra (n)", y = "Diferenças Médias", title = "Diferença entre Intervalos de Confiança") +
    theme_minimal()

# Salvar o gráfico num arquivo JPEG
ggsave("9-Grafico.jpeg", plot, width = 21, height = 15, units = "cm")
