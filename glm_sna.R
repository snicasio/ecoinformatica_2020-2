data("mtcars")

head(mtcars)

        ##   GLM binomial

md_bin <- glm(vs ~ wt, data=mtcars)
    md_bin
    summary(md_bin)

        ##   Disribucion binomial


summary(as.factor(mtcars$vs))

n <- 32 # No. total de eventos
x <- 14 # No. eventos exitosos
p <- 0.7 # Probabilidad de exito

Po <- (factorial(n)/(factorial(n-x)*factorial(x)))*(p^x)*(1-p)^(n-x)
Po

p <- seq(0.01,0.999,0.01);p

tab_po <- data.frame(p,Po = rep(NA,99));tab_po
    for(i in 1:nrow(tab_po)){
        tab_po[i,2] <- (factorial(n)/(factorial(n-x)*factorial(x)))*(tab_po[i,1]^x)*(1-tab_po[i,1])^(n-x)
    }
tab_po
plot(tab_po)    #   Probabilidad de ocurrencia en funcion de la probabilidad de exito


        ##  Funcion de probabilidad logaritmica


log(factorial(n)/(factorial(n-x)*factorial(x))) + x*log(p) + (n-x) * log(1-p)

tab_po$LL <- log(factorial(n)/(factorial(n-x)*factorial(x))) + x*log(p) + (n-x) * log(1-p)

#Log-Likelihood

par(mfrow = c(1,2))
    plot(tab_po[,-3])
    plot(tab_po[,-2])


max(tab_po$LL)

        ##   MLE (Maximum Likelihood Estimator)

tab_po[tab_po$LL == max(tab_po$LL), ]


        ##  GLM Log-Likelihood

md_bin
logLik(md_bin)


md_wt <- glm(vs ~ wt, data=mtcars, family = binomial)
md_mpg <- glm(vs ~ mpg, data=mtcars, family = binomial)
md_hp <- glm(vs ~ hp, data=mtcars, family = binomial)
md_drat <- glm(vs ~ drat, data=mtcars, family = binomial)
md_qsec <- glm(vs ~ qsec, data=mtcars, family = binomial)
md_null <- glm(vs ~ 1, data=mtcars, family = binomial)

mod_lista <- list(Nulo=md_null,
                  Peso=md_wt,
                  MPG=md_mpg,
                  Pot=md_hp,
                  DRT=md_drat,
                  Ace=md_qsec)

lapply(mod_lista, logLik)



        ##  Devianza

summary(md_null)    #   Devianza nula
summary(md_qsec)    #   Devianza en funcion de una variable independiente
summary(md_drat)


anova(md_qsec, test = "Chisq")  #   Prueba de bondad de ajuste
anova(md_drat, test = "Chisq")  #   Prueba de bondad de ajuste
anova(md_null, test = "Chisq")  #   Prueba de bondad de ajuste


        ##  Parsimonia

            ### Akaike Index Criterion (AIC)

# AIC = -2*loglik + 2K

(-2*(-21.93005))+(2*1)

lapply(mod_lista, AIC)  #   Pareciera que la informacionel AIC es redundante respecto al LL. Sin embargo, esto cambia cuando elaboramos modelos con mas de una variable independiente


# Elaboren modelos que incluyan todas las combinaciones de dos y tres variables independientes. Pueden usar funciones o loops para ello

var_ind <- names(mtcars)[c(1,3:7)]
combi <- c(list(1,2,3,4,5,6),combn(1:6,2, simplify = F),combn(1:6,3, simplify = F))


paste0("vs~", paste(var_ind[combi[[20]]], collapse = "+"))

mod_name <- list()
    for(i in seq(combi)){
        mod_name[[i]] <- paste0("vs~",paste(var_ind[combi[[i]]],collapse = "+"))
        names(mod_name)[i] <- paste(var_ind[combi[[i]]],collapse = "+")
    }

modelos <- lapply(mod_name, function(x) glm(x,data=mtcars))

modelos$nulo <- glm(vs ~ 1, data=mtcars, family = binomial)


#unclass(logLik(modelos$mpg))[1]



tab_mod <- data.frame(LL = sapply(modelos, function(x) unclass(logLik(x))[1]),
                      AIC = sapply(modelos, AIC))
    
plot(tab_mod)
    

            ### Numero de parametros (K)

# Modelos nulo: ordenada al origen y el error (2)
# Modelo simple: ordenada al origen, B1 y el error (3)
# Modelo con dos variables independientes: ordenada al origen, B1, B2 y el error (4)

tab_mod[1,1]

            ### Vamos a calcular el AIC de acuerdo a la formula

((-2)*tab_mod[1,1])+(2*3)


            ### Ahora calculemos el AIC corregido (AICc)

nrow(mtcars)

tab_mod[1,2]+((2*3*(3+1))/(32-3-1))


            ### Diferencias AIC

min(tab_mod$AIC)
sort(tab_mod$AIC)

tab_mod1 <- tab_mod[order(tab_mod$AIC),]
    tab_mod1


tab_mod1[1,2]
tab_mod1$AIC_delta <- tab_mod1$AIC-tab_mod1[1,2]
tab_mod1


plot(tab_mod1[,-2])


            ### Diferencias AICc

tab_mod$K <- sapply(modelos,function(x) length(x$coefficients)+1)

tab_mod$AICc <- tab_mod[,"AIC"]+(((2*tab_mod[,"K"])*(tab_mod[,"K"]+1))/(32-tab_mod[,"K"]-1))

tab_mod$delta_AIC <- tab_mod$AIC-min(tab_mod$AIC)

tab_mod$delta_AICc <- tab_mod$AICc-min(tab_mod$AICc)

tab_mod

tab_mod[order(tab_mod$delta_AICc),]


        ###   Pesos AIC y AICc

tab_mod$w_aic <- exp(-(1/2)*(tab_mod$AIC))/sum(exp(-(1/2)*(tab_mod$AIC)))

tab_mod$w_aicc <- exp(-(1/2)*(tab_mod$AICc))/sum(exp(-(1/2)*(tab_mod$AICc)))


        ##  Bayesian Index Criterion (BIC)

# K * log(n) - 2log(LL)

tab_mod$BIC <- tab_mod$K*log(32)-(2*tab_mod$LL)

tab_mod$delta_BIC <- tab_mod$BIC-min(tab_mod$BIC)

tab_mod[order(tab_mod$delta_BIC),c("delta_AIC","delta_AICc","delta_BIC")]


any(mtcars$vs %in% 0:1)



mdsel_sna <- function(dep,ind,parm,data,type){
    
    var_ind <- ind
    combi <- c(list(1,2,3,4,5,6),combn(seq(ind),2, simplify = F),combn(seq(ind),3, simplify = F))
    
    mod_name <- list()
    for(i in seq(combi)){
        mod_name[[i]] <- paste0(dep,"~",paste(var_ind[combi[[i]]],collapse = "+"))
        names(mod_name)[i] <- paste(var_ind[combi[[i]]],collapse = "+")
    }
    
    modelos$nulo <- glm(dep ~ 1, data=data, family = binomial)
    
    if(is.double(dep) == T & type == "glm"){
        md <- lapply(mod_name, function(x) glm(x,data=mtcars, family = gaussian))
    }else{
        if(is.integer(dep) == T & type == "glm"){
            md <- glm(family = poisson)
        }else{
            if(any(dep %in% 0:1) == TRUE & type == "glm"){
                md <- glm(family = binomial)
            }
        }
    }else{
        if(is.double(dep) == T & type == "lm"){
            md <- lm()
        }
    }
    LL <- unclass(logLik(md))[1]
    AIC <- AIC(md)
    AICc
    
    return(data.frame(Nombre,K,LL,AIC,AICc,BIC,delta_AIC,delta_AICc,delta_BIC,w_AIC,w_AICc))
}




