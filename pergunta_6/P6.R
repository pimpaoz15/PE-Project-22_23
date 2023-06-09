# Resposta Excel ->

####################################################################################################
# Função de probabilidade da distribuição de Benford
prob_benford <- function(x) {
    return(log10(1 + 1 / x))
}

# 1) Probabilidade de X ser igual a 3 ou 6
P_X_3 <- prob_benford(3)
P_X_8 <- prob_benford(8)
P_X_3_ou_8 <- P_X_3 + P_X_8

# 2) Fração de potências de dois no intervalo [2^8, 2^28] cujo primeiro algarismo é igual a 3 ou 6
total_potencias <- 0
potencia <- 1

while (potencia <= 24) {
    numero <- 2^potencia
    primeiro_algarismo <- as.integer(substr(as.character(numero), 1, 1))

    if (primeiro_algarismo == 3 || primeiro_algarismo == 8) {
        total_potencias <- total_potencias + 1
    }

    potencia <- potencia + 1
}

fração <- total_potencias / (24 - 1 + 1)

# 3) Cálculo do desvio absoluto
desvio_absoluto <- abs(fração - P_X_3_ou_8)

# 4) Arredondamento do desvio absoluto
desvio_arredondado <- round(desvio_absoluto, 4)

# Resultados
print(paste("Probabilidade de X ser igual a 3 ou 6:", P_X_3_ou_8))
print(paste("Fração de potências de dois no intervalo com primeiro algarismo igual a 3 ou 6:", fração))
print(paste("Desvio absoluto:", desvio_absoluto))
print(paste("Desvio arredondado:", desvio_arredondado))
