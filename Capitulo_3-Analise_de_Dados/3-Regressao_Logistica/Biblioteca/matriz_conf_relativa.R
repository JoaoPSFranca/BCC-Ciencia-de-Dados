matriz_conf_relativa <- function(mat_conf, nomelinha="Classe", nomecoluna="Predição"){

    mcrel  <- mat_conf
    margem <- margin.table(mat_conf,1)

    mcrel[1,1] = round(mat_conf[1,1]/margem[1], digits=3)
    mcrel[1,2] = round(mat_conf[1,2]/margem[1], digits=3)
    mcrel[2,1] = round(mat_conf[2,1]/margem[2], digits=3)
    mcrel[2,2] = round(mat_conf[2,2]/margem[2], digits=3)
    names(dimnames(mcrel))[1] <- nomelinha
    names(dimnames(mcrel))[2] <- nomecoluna
    
    return(mcrel)  
}
