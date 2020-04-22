
#gradientes de abundancias afectados por gradiente ambientales
 #exploratorios : describir el gradiente (PCA,NMDS,CA)
 #interpretación ambiental : variable explicativas -> influencia en
                                         # los arreglos de las comunidades (RDA, CCA)

#   % de Variación explicada

#RDA : lineal (transformación de abundancias para dist. norm)// 
#CCA : Unimodal (óptimo)  

library(vegan)

#RDA: combines regression and PCA 
# It is constrained because you are directly testing 
#the influence of explanatory variables.

abu <- read.csv("LL_abu.csv")[ ,-1:-2]
env <- read.csv("LL_env.csv")[ ,-1:-2]
mic <- read.csv("LL_micro.csv")[ ,-1:-2]

r_a <- radfit(colSums(abu), model = "Lognormal") #Rango abundancia

radlattice(r_a)

names(r_a$y[r_a$y > 3])
r_a$y

abu
abu_trans <- decostand(abu, method = "max") #estandarización : max -> divide by margin maximum
abu_pca <- prcomp(abu_trans) #PCA
cum_abu <- summary(abu_pca)$importance[3,2]

#PCA para ver si la var. expl. aumenta a medida que se quitan
#especies generalistas o insuficientes (-3 o 5 ocurrencias) 10% más-menos abundantes
#10% 20%

#decorana (DCA) -> eigen values para elegir el mejor escenario 

r_a$y

abu_lista <- list(Todas = ncol(abu),
                  Gen1 = abu[ ,-grep("Nardstri", names(abu))],
                  Esp0 = abu[ ,names(r_a$y[r_a$y > 1])],
                  Esp1 = abu[ ,names(r_a$y[r_a$y > 2])],
                  Esp2 = abu[ ,names(r_a$y[r_a$y > 3])],
                  G1E1 = abu[ ,names(r_a$y[r_a$y < 500 & r_a$y > 2])],
                  G1E2 = abu[ ,names(r_a$y[r_a$y < 500 & r_a$y > 3])],
                  G2E1 = abu[ ,names(r_a$y[r_a$y < 300 & r_a$y > 2])],
                  G2E2 = abu[ ,names(r_a$y[r_a$y < 300 & r_a$y > 3])])

abu_trans <- lapply(abu_lista, decostand, method="max")
abu_pca <- lapply(abu_trans, prcomp)

cum_abu <- sapply(abu_pca, function(x) summary(x)$importance[3,2])
cum_abu #varianza explicada

abu_lista$Esp2 # <- El mejor conjunto de datos

# Each RDA axis has an eigenvalue, which is the 
# proportion of the variance explained by each axis


## CCA trabaja directo con los valores de abundancia 
# inercia -> diferencia ente ab. rel. respecto a lo esperado (distribución equitativa)  MAYOR INERCIA, MAYORES dif de Proporciones
# uncons  = composición de sp en c/ sitio
# cons = diferencias en las proporciones explicadas por var. amb. 28%

CCA_abu <- cca(abu_lista$Esp2 ~ ., data = env)
CCA_abu
summary(CCA_abu)
plot(CCA_abu)

ordistep(CCA_abu)

cca(abu_lista$Esp2 ~ AWTD, data = env)

##transformar variables ambientales

normal <- function(x){
    if(shapiro.test(x)$p.value >= 0.05){
        return("SI")
    }else{
        return("NO")
    }
}
normal(ph)


P<- sqrt(env$Phosphorous)# sqrt
Ca<- log(env$Calcium) #log
Mg<-log(env$Magnesium)# log

ph<-(env$pH) #log :c
K<- (env$Potassium) #log
Na<-(env$Sodium)# log

library(moments)
sesgo<-apply(env, MARGIN=2, skewness)

ifelse(sesgo > 0 & sesgo <= 1, "sqrt(x)",
       ifelse(sesgo > 1, "log(x)",
              ifelse(sesgo < 0 & sesgo >= -1, "x^2",
                     ifelse(sesgo < -1, "exp(x)", "NONE"))))




