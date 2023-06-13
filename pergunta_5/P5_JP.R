# seed
set.seed(1235)

n <- 1139

p <- 0.5

xsim <- numeric()

i <- 0

while (length(xsim) != n) {
    u <- runif(1, 0, 1) # gerar valor de u
    if (i == 0) {
        if (u <= pgeom(0, p)) {

        } else {
            i <- i + 1
            while (((pgeom(i - 1, p) < u) && (u <= pgeom(i, p))) == FALSE) {
                i <- i + 1
            }
        }
        xsim <- c(xsim, i)
        i <- 0
    }
}

mean_plus_sd <- mean(xsim) + sd(xsim)
a <- length(which(xsim > mean_plus_sd))
b <- length(which(xsim > mean(xsim)))

final <- a / b
print(final)
