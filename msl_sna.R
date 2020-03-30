msel_sna <- function(dep,ind,parm,data,type){
    
    var_ind <- ind
    combi <- do.call(c, lapply(seq(ind), function(x) combn(seq(ind),x, simplify = F)))
    
    mod_name <- list()
    for(i in seq(combi)){
        mod_name[[i]] <- paste0(dep,"~",paste(var_ind[combi[[i]]],collapse = "+"))
        names(mod_name)[i] <- paste(var_ind[combi[[i]]],collapse = "+")
    }
    mod_name$nulo <- paste0(dep,"~", "1")

    if(is.double(data[,dep]) == T & type == "glm"){
        md <- lapply(mod_name, function(x) glm(x,data=data, family = gaussian))
    }else{
        if(is.integer(data[,dep]) == T & type == "glm"){
            md <- lapply(mod_name, function(x) glm(x,data=data, family = poisson))
        }else{
            if(any(data[,dep] %in% 0:1) == TRUE & type == "glm"){
                md <- lapply(mod_name, function(x) glm(x,data=data, family = binomial))
            }else{
                if(is.double(data[,dep]) == T & type == "lm"){
                    md <- lapply(mod_name, function(x) lm(x,data=data))
                }
            }
        }
    }
    Nombre <- names(mod_name)
    LL <- sapply(md,function(x) unclass(logLik(x))[1])
    AIC <- sapply(md,AIC)
    K <- sapply(md,function(x) length(x$coefficients)+1)
    AICc <- AIC+(((2*K)*(K+1))/(nrow(data)-K-1))
    delta_AIC <- AIC-min(AIC)
    delta_AICc <- AICc-min(AICc)
    w_aic <- exp(-(1/2)*(AIC))/sum(exp(-(1/2)*(AIC)))
    w_aicc <- exp(-(1/2)*(AICc))/sum(exp(-(1/2)*(AICc)))
    BIC <- K*log(nrow(data))-(2*LL)
    delta_BIC <- BIC-min(BIC)
    
    return(data.frame(Nombre,K,LL,AIC,AICc,BIC,delta_AIC,delta_AICc,delta_BIC,w_aic,w_aicc))
}




