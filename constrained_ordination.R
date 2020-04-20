library(vegan)

abu <- read.csv("LL_abu.csv")[ ,-1:-2]
env <- read.csv("LL_env.csv")[ ,-1:-2]
mic <- read.csv("LL_micro.csv")[ ,-1:-2]



r_a <- radfit(colSums(abu), model = "Lognormal")


radlattice(r_a)

names(r_a$y[r_a$y > 3])




abu
    abu_trans <- decostand(abu, method = "max")
    abu_pca <- prcomp(abu_trans)
    cum_abu <- summary(abu_pca)$importance[3,2]


abu1 <- abu[ ,-grep("Nardstri", names(abu))]
    abu1_trans <- decostand(abu1, method = "max")
    abu1_pca <- prcomp(abu1_trans)
    cum_abu1 <- summary(abu1_pca)$importance[3,2]


tabla_var <- data.frame(comm = c("Todas","Gen10"),
                        var = c(cum_abu,cum_abu1))

plot(tabla_var)





abu_lista <- list(Todas = abu,
                  Gen1 = abu[ ,-grep("Nardstri", names(abu))],
                  Gen2 = abu[ ,-grep("Nardstri|Carengir", names(abu))],
                  Esp1 = abu[ ,names(r_a$y[r_a$y > 2])],
                  Esp2 = abu[ ,names(r_a$y[r_a$y > 3])],
                  G1E1 = abu[ ,names(r_a$y[r_a$y < 500 & r_a$y > 2])],
                  G1E2 = abu[ ,names(r_a$y[r_a$y < 500 & r_a$y > 3])],
                  G2E1 = abu[ ,names(r_a$y[r_a$y < 300 & r_a$y > 2])],
                  G2E2 = abu[ ,names(r_a$y[r_a$y < 300 & r_a$y > 3])])

abu_trans <- lapply(abu_lista, decostand, method="max")
abu_pca <- lapply(abu_trans, prcomp)

cum_abu <- sapply(abu_pca, function(x) summary(x)$importance[3,2])
cum_abu




abu_lista$Esp2

CCA_abu <- cca(abu_lista$Esp2 ~ ., data = env)
CCA_abu
summary(CCA_abu)
plot(CCA_abu)

ordistep(CCA_abu)

cca(abu_lista$Esp2 ~ AWTD, data = env)
