    #   EVALUACION

library(microbenchmark)

library(ggplot2)
library(vegan)
data("BCI")

source("primer_parcial.R")
library(ARZ)
library(vrm)
library(KJLM)
library(CRRL)

        ##  Numeros de Hill

ARZ::divHill(BCI)
CRRL::Hill(BCI)
KJLM::Diversidad_Hill(as.matrix(BCI))
vrm::Hill_num(BCI)

hill_eval <- microbenchmark(hill_sna(BCI),divHill(BCI),Hill(BCI),Hill_num(BCI),times = 500)
    levels(hill_eval$expr) <- c("Nicasio","Aurora","Claudio","Valeria")
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


            ### Suma acumulada

CRRL::Sumac(1:100)

acc_eval <- microbenchmark(cumsum(1:100),sum_acc_sna(1:100),Sumac(1:100), times = 500)
    levels(acc_eval$expr) <- c("R","Nicasio","Claudio")
    acc_eval
    autoplot(acc_eval)


            ### Factorial

KJLM::Factorial(10)
vrm::fct(10)

factorial_eval <- microbenchmark(factorial(10),fct_sna(10),Factorial(10),fct(10), times = 500)
    levels(factorial_eval$expr) <- c("R","Nicasio","Kevin","Valeria")
    factorial_eval
    autoplot(factorial_eval)
