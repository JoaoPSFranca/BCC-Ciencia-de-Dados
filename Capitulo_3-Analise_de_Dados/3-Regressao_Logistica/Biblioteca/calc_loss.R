calc_loss <- function(real, pred){
    real <- as.numeric(real == 1)
    somaLoss <- 0.0
    epsilon  <- 1.0e-9  # fator de estabilidade numérica para evitar log2(0)
    for(i in 1:length(real)){
        fn <- (1 - real[i]) * log2(1 - pred[i] + epsilon)
        fp <-      real[i]  * log2(    pred[i] + epsilon)
        somaLoss <- somaLoss + fn + fp
    }
    return( -somaLoss / length(real))
}

calc_loss_bal <- function(real, pred) {
    real <- as.numeric(real == 1)
    
    so_neg_real <- real[real == 0]
    so_neg_pred <- pred[real == 0]
    so_pos_real <- real[real == 1]
    so_pos_pred <- pred[real == 1]
    loss_neg    <- calc_loss(so_neg_real, so_neg_pred)
    loss_pos    <- calc_loss(so_pos_real, so_pos_pred)
    
    return ((loss_neg + loss_pos) / 2.0)
}
