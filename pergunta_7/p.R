# Resposta Excel ->

####################################################################################################
set.seed(1114) # Fixar a semente
m <- 1248
n <- 10
p <- 0.47

Y <- matrix(NA, m, n, TRUE)
for (i in 1:m) {
    for (j in 1:n) {
        Y[i, j] <- rnorm(1, 0, 1)
    }
}

# diferença entre o quantil de probabilidade 0.47 e
# quantil correspondente à distribuição teórica da soma de quadrados de variáveis normais reduzidas independentes

quantil_047 <- quantile(Y, probs = p, type = 2) # quantil de probabilidade 0.47
quantil <- qchisq(p, df = 1, ncp = 0)
Resultado <- quantil_047 - quantil
Resultado <- round(Resultado, digits = 4)

print(Resultado)
