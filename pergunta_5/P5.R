# Resposta Excel ->

####################################################################################################
set.seed(1235) # Fixar a semente
N <- 1139 # numero de amostras
p <- 0.5

# função de distribuição
fx <- rep(0, N) # inicializar um vetor a zeros com N posições
fx[1] <- ((1 - p)^(1 - 1)) * p
for (i in 2:N) {
    fx[i] <- ((1 - p)^(i - 1)) * p + fx[i - 1]
}

# amostragem
X <- rep(0, N) # inicializar um vetor a zeros com N posições
i <- 1
while (i <= N) {
    u <- rnorm(1, 0, 1)
    if (u >= 0 & u <= 1) {
        buffer <- 1
        for (x in 2:N) {
            if (u > fx[x - 1] & u <= fx[x]) {
                buffer <- x
            }
        }
        X[i] <- buffer
        i <- i + 1
    }
}

# média
mean <- mean(X)

# desvio padrão amostral
dp <- sd(X)

# selecionar os valores superiores á média
superior_media <- 0
sup_media_desvio <- 0
for (i in 1:N) {
    if (X[i] > mean) {
        superior_media <- superior_media + 1
        if (X[i] > mean + dp) {
            sup_media_desvio <- sup_media_desvio + 1
        }
    }
}

# porção dos valores que são maiores que a soma da média com o desvio padrão amostral,
# de entre os que são superiores à respetiva média amostral
Resultado <- sup_media_desvio / superior_media
round(Resultado, digits = 4)
