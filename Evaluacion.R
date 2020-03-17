    #   EVALUACION

library(microbenchmark)
library(ggplot2)
library(vegan)
data("BCI")

source("primer_parcial.R")
library(ARZ)
library(vrm)


        ##  Numeris de Hill

ARZ::divHill(BCI)
vrm::Hill_num(BCI)

hill_eval <- microbenchmark(hill_sna(BCI),divHill(BCI),Hill_num(BCI),times = 500)
    levels(hill_eval$expr) <- c("Nicasio","Aurora","Valeria")
    hill_eval
    autoplot(hill_eval)


        ##  Funcion extra

            ### Valores unicos

num_rep <- c(1,1,2,3,3,4:10,11,11,11,12,12,13:30,31,31,31)

ARZ::unicos(num_rep)

unicos_eval <- microbenchmark(unique(num_rep),unicos(num_rep), times = 500)
    levels(unicos_eval$expr) <- c("R","Aurora")
    unicos_eval
    autoplot(unicos_eval)


            ### Factorial

vrm::fct(10)

factorial_eval <- microbenchmark(factorial(10),fct_sna(10),fct(10), times = 500)
    levels(factorial_eval$expr) <- c("R","Nicasio","Valeria")
    factorial_eval
    autoplot(factorial_eval)
