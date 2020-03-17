    #   EVALUACION

library(microbenchmark)
library(vegan)
data("BCI")

require()
library(ARZ)
library(vrm)

ARZ::divHill(BCI)
vrm::Hill_num(BCI)

ARZ::unicos(c(1,1,2,3,3,4,5,6))
vrm::fct(5)

