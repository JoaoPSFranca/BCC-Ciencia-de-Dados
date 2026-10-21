
acuracia <- function (mat_conf) {
    x <- as.matrix(mat_conf)
    acc <- (x[2,2] + x[1,1]) / (x[2,2] + x[1,1] + x[2,1] + x[1,2])
    return (acc)
}

acuracia_balanceada <- function (mat_conf) {
    mat <- as.matrix(mat_conf)
    tn <- mat[1,1]
    tp <- mat[2,2]
    fn <- mat[2,1]
    fp <- mat[1,2]
    tpr <- tp / (tp + fn)
    tnr <- tn / (tn + fp)
    return ((tpr + tnr) / 2)
}
