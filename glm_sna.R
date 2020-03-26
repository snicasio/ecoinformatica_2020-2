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

mod_name <- list()
    for(i in seq(combi)){
        mod_name[[i]] <- paste0("vs~",paste(var_ind[combi[[i]]],collapse = "+"))
        names(mod_name)[i] <- paste(var_ind[combi[[i]]],collapse = "+")
    }

modelos <- lapply(mod_name, function(x) glm(x,data=mtcars))

tab_mod <- data.frame(LL = sapply(modelos, function(x) unclass(logLik(x))[1]),
                      AIC = sapply(modelos, AIC))
    

    
    





