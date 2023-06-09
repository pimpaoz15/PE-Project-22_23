# RESPOSTA EXCEL ->

############################################################################################################
# Set seed
set.seed(1275)

# Generate sample
sample <- rexp(2493, rate = 12.5)

# Set parameters
sj <- cumsum(sample)
T <- ceiling(sj[2493])

# Set unitary subintervals
subintervals <- seq(1, T, by = 1)
count <- table(cut(sj, breaks = subintervals, right = FALSE)) # right = FALSE argument specifies that the intervals should be left-closed and right-open.
obs_mean <- mean(count)
absolute_deviation <- abs(obs_mean - 12.5)
absolute_deviation <- round(absolute_deviation, 4)

# Print result
print(absolute_deviation)


############################################################################################################
# set seed
set.seed(1275)

# generate sample
amostra <- rexp(2493, rate = 12.5)

# set parameters
sj <- cumsum(amostra)
T <- ceiling(sj[2493])

# set unitary subintervals
subintervalos <- seq(1, T, by = 1)
contagem <- table(cut(sj, breaks = subintervalos, right = FALSE)) # right = FALSE argument specifies that the intervals should be left-closed and right-open.
media_obs <- mean(contagem)
media_teorica <- length(sj) / T
desvio_absoluto <- abs(media_obs - media_teorica)
desvio_absoluto <- round(desvio_absoluto, 4)

# print result
print(desvio_absoluto)
