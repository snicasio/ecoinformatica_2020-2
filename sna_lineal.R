data("iris")

holi <- iris[iris$Species=="setosa",]
names(holi)

source("loocv.R")

cv(x=holi,dep = "Sepal.Length", ind = "Sepal.Width")
