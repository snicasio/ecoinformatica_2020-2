data("iris")
names(iris)


mod_lin <- lm(Petal.Length ~ Sepal.Length, data = iris[iris$Species=="virginica",])
    mod_lin
    summary(mod_lin)
    anova(mod_lin)

mod_lin2 <- lm(Sepal.Width ~ Sepal.Length, data = iris[iris$Species=="virginica",])
    mod_lin2
    summary(mod_lin2)
    anova(mod_lin2)


par(mfrow = c(1,2))
    plot(Petal.Length ~ Sepal.Length, data = iris[iris$Species=="virginica",])
        abline(mod_lin, lwd = 2, col="red")
        abline(h = mean(iris[iris$Species=="virginica",]$Petal.Length), lwd = 2, col="blue")
    
    plot(Sepal.Width ~ Sepal.Length, data = iris[iris$Species=="virginica",])
        abline(mod_lin2, lwd = 2, col="red")
        abline(h = mean(iris[iris$Species=="virginica",]$Sepal.Width), lwd = 2, col="blue")


residuals(mod_lin)

mean(residuals(mod_lin))
shapiro.test(residuals(mod_lin))
hist(residuals(mod_lin))

    #   VALIDACIONES CRUZADAS

        ##   Leave-one-out cross-validation


vir <- iris[iris$Species=="virginica",]
vir[1,"Petal.Length"]   #   Valor a omitir (dato de validacion)
vir[-1,]    #   Datos de calibracion

mod1 <- lm(Petal.Length ~ Sepal.Length, data = vir[-1,])
summary(mod1)
predict(mod1)   #   Los valore de la linea de regresion

#y=mx+b
predict(mod1)  #   Podemos predecir valores de una tabla de datos
predict(mod1,vir)   #   Aca incluyo la observacion que habia omitido
predict(mod1,vir)[1]    #   Prediccion de la observacion omitida

### Elaboremos su valor de error

6-5.319518  #   Error de prediccion
(6-5.319518)^2  #   Error de prediccion al cuadrado

#MSE

cv <- function(x,dep,ind){
    val <- rep(NA,nrow(x))
    for(i in 1:nrow(x)){
        val[i] <- predict(lm(paste0(dep, "~", ind) , data = vir[-i,]), vir)[i]
    }
    error <- mean((val-x[,dep])^2)
    Rcv <- 1-(mean((x[,ind]-mean(x[,ind]))^2))
    R <- summary(lm(paste0(dep, "~", ind) , data = x))$adj.r.squared
    return(data.frame(error,R,Rcv))
}

cv(x=vir,dep = "Petal.Length",ind = "Sepal.Length")

