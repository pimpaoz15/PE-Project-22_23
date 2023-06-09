# Resposta Excel ->

####################################################################################################
set.seed(1055) # Fixar a semente
m <- 1507
n <- 18
p <- 0.31

# amostra
X <- matrix(rnorm(m * n, 0, 1), nrow = m, ncol = n)

# Para cada uma das amostras, calcule a soma dos quadrados dos valores observados
Y <- rep(0, m)
for (i in 1:m) {
    for (j in 1:n) {
        Y[i] <- Y[i] + X[i, j]
    }
}

# diferença entre o quantil de probabilidade 0.47 e
# quantil correspondente à distribuição teórica da soma de quadrados de variáveis normais reduzidas independentes

quantil_047 <- quantile(Y, probs = p, type = 2) # quantil de probabilidade 0.47
quantil <- qchisq(p, df = 1, ncp = 0)
Resultado <- quantil_047 - quantil
Resultado <- round(Resultado, digits = 4)

print(Resultado)
