# média dos erros absolutos
media_erros_abs <- function (observado, predito) {
    resultado <- sum(abs(observado - predito))
    return (resultado / length(observado))
}

# média quadrados dos erros
media_erros_quad <- function (observado, predito) {
    resultado <- sum((observado - predito)^2)
    return (resultado / length(observado))
}
