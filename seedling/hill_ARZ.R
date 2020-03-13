
divHill<-function(x){
    piprom <- colMeans(as.matrix(x/rowSums(x))) #Abundancias relativas promedio
    gamma0<-(sum((piprom)^0))^(1/(1-0)) #gamma orden 0
    gamma1<- exp(- sum( (piprom) * log(piprom))) #gamma orden 1
    gamma2<-(sum((piprom)^2))^(1/(1-2)) ##gamma orden 2
    pi <- as.matrix(x/rowSums(x))
    a0 <- ifelse(pi == 0, 0, pi^0) # Los valores que originalmente son cero, se mantienen como cero. De otra forma, se elevan a cero
    alpha0 <- sum(colMeans(a0))^(1/(1-0))
    a1 <- as.matrix(pi*log(pi)) # Producto de las abundancias relativas por su logaritmo natural (NOTA: apareceran NaN's)
    a1[is.nan(a1)==T] <- 0 # Cambiar los NaN por ceros (empleo una sola dimension porque es una matriz)
    alpha1 <- exp(-mean(rowSums(a1))) #alpha orden 1
    alpha2 <- sum(colMeans(pi^2))^(1/(1-2))
    beta0<-gamma0/alpha0
    beta1<-gamma1/alpha1
    beta2<-gamma2/alpha2
    
    return(data.frame(
        "Orden" = c("0", "1", "2"),
        "Gamma" = round(c(gamma0,gamma1,gamma2),3), 
        "Alpha" = round(c(alpha0,alpha1,alpha2),3), 
        "Beta" = round(c(beta0,beta1,beta2),3) ) )
    
}

unicos <- function(x){
    u <- x [duplicated(x) == F]
    return(u)
}



