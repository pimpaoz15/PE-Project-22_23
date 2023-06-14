# Dados do problema
set.seed(1065) # Fixar a semente
m <- 1507
n <- 18
p <- 0.31

# amostra
X <- matrix(rnorm(m * n, 0, 1), nrow = m, ncol = n)

# Para cada uma das amostras, calcule a soma dos quadrados dos valores observados
Y <- rep(0, m)
for (i in 1:m) {
    for (j in 1:n) {
        Y[i] <- Y[i] + X[i, j]^2
    }
}

# diferença entre o quantil de probabilidade 0.47 e
# quantil correspondente à distribuição teórica da soma de quadrados de variáveis normais reduzidas independentes

quantil_Y <- quantile(Y, probs = p, type = 2) # quantil de probabilidade p
quantil_teorica <- qchisq(p, df = n) # quantil de probabilidade p teorico
Resultado <- quantil_Y - quantil_teorica
Resultado <- round(Resultado, digits = 4)
print(paste("Resultado:", abs(Resultado)))
