# padronização ZNorm: x' = (x - média(x)) / desvio(x)
padroniz_znorm <- function (x) {
    xmedia  <- mean(x)
    xdesvio <- sd(x)
    return ((x - xmedia) / xdesvio)
}

# padronização min_max: x' = (x - min(x)) / (max(x) - min(x))
padroniz_minmax <- function (x) {
    xmin <- min(x)
    xmax <- max(x)
    ampl <- xmax - xmin
    return ((x - xmin) / ampl)
}
