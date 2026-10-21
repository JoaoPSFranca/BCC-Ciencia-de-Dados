formula_potencia <- function (var_dep, base, variaveis, ate_potencia=5) {
    frm <- character(ate_potencia)
    b <- ""
    if (length(base) != 1 | trimws(base[1]) != "") {
        b <- ""
        for (i in 1:length(base))
            b <- paste(b,base[i],"+",sep=" ")
    }
    base <- b
    for (i in 1:ate_potencia) {
        mais <- ""
        f <- ""
        for (j in 1:length(variaveis)) {
            if (i == 1) {
                f <- paste(f, mais," ", variaveis[j], sep="")
            }
            else {
                f <- paste(f, mais, " I(",variaveis[j],"^",i,")", sep="")
            }
            mais <- " +"
        }
        if (i == 1) {
            frm[i] <- paste(base, f)
        }
        else {
            frm[i] <- paste(frm[i-1], " +", f, sep="")
        }
    }
    for (i in 1:ate_potencia) 
        frm[i]    <- paste(var_dep, " ~", frm[i], sep="")
    return (frm)
}
