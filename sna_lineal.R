data("iris")

holi <- iris[iris$Species=="virginica",]
names(holi)

source("loocv.R")

cv(x=holi,dep = "Petal.Length", ind = "Sepal.Length")
