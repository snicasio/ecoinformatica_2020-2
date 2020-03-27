data("iris")

holi <- iris[iris$Species=="virginica",]
names(holi)

source("loocv.R")

RES <- lapply(levels(iris$Species),
              function(x)
                  lapply(c("Petal.Length", "Petal.Width"),
                         function(y)
                             do.call(rbind,lapply(names(iris)[c(-3,-5)],
                                                  function(z)
                                                      cv(iris[iris$Species == x,],dep = y, ind = z)))))
names(RES) <- levels(iris$Species)
    for(i in seq(RES))
        for(j in seq(RES[[i]]))
            for(k in seq(RES[[i]][[j]])){
                names(RES[[i]]) <- c("Petal.Length", "Petal.Width")
                rownames(RES[[i]][[j]]) <- names(iris)[c(-3,-5)]
        
    }








cv(x=holi,dep = "Petal.Length", ind = "Sepal.Length")





