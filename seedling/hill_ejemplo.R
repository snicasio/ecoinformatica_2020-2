library(vegan)
data("BCI")

    #   PARA OBTENER LA DIVERSIDAD ALFA CUANDO q=0


bci_rel <- as.matrix(BCI/rowSums(BCI))

a0 <- ifelse(bci_rel == 0, 0, bci_rel^0) # Los valores que originalmente son cero, se mantienen como cero. De otra forma, se elevan a cero

alfa_0 <- sum(colMeans(a0))^(1/(1-0)) #  Formula
    alfa_0 #    Listo

    #   PARA OBTENER LA DIVERSIDAD ALFA CUANDO q=1

bci_rel <- as.matrix(BCI/rowSums(BCI))

a1 <- as.matrix(bci_rel*log(bci_rel)) # Elaboro un objeto donde se calcule el producto de las abundancias relativas por su logaritmo natural (NOTA: apareceran NaN's)
    a1[is.nan(a1)==T] <- 0 #    Este codigo es para cambiar los NaN por ceros (empleo una sola dimension porque es una matriz)

alfa_1 <- exp(-mean(rowSums(a1))) # Formula
    alfa_1 #    Listo

    #   ESTOS SON LOS VALORES PROMEDIO DE ALFA PARA UNA SOLA MATRIZ. ESE VALOR ES EL QUE DIVIDE A GAMMA PARA OBTENER LA BETA
