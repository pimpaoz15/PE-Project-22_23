# Seed
set.seed(1275)
# Geração da amostra (os valores correspondem aos tempos entre acontecimentos sucessivos)
data <- rexp(2493, rate = 12.5)
# t corresponde ao instante da ocorrência do 1820-ésimo acontecimento (somatório dos tempos)
t <- Reduce("+", data) # Como 1820 é o ultimo termo, basta somar todos os elementos do array
t <- ceiling(t) # inteiro mais proximo acima

vect <- numeric() # contem o instante em que cada acontecimento ocorre

for (i in 1:length(data)) {
    if (i == 1) {
        vect[i] <- data[i]
    } else {
        vect[i] <- data[i] + vect[i - 1]
    }
}

vect2 <- cut(vect, 0:t) # dividir o vetor em t intervalos unitarios
o_table <- table(vect2) # colocar numa tabela o numero de ocurrencias em cada intervalo
media <- mean(o_table)
desvio_abs <- abs(media - 12.5)
print(desvio_abs)
