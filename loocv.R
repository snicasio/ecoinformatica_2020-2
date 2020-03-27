cv <- function(x,dep,ind){
    mod <- lm(paste0(dep, "~", ind) , data = x)
    val <- rep(NA,nrow(x))
    for(i in 1:nrow(x)){
        val[i] <- predict(lm(paste0(dep, "~", ind) , data = x[-i,]), x)[i]
    }
    error <- mean((val-x[,dep])^2)
    ind_mss <- (1/nrow(x))*(sum((x[,dep]-mean(x[,dep]))^2))
    Rcv <- 1-(error/ind_mss)
    R <- summary(mod)$adj.r.squared
    return(data.frame(error,R,Rcv))
}