cv <- function(x,dep,ind){
    val <- rep(NA,nrow(x))
    for(i in 1:nrow(x)){
        val[i] <- predict(lm(paste0(dep, "~", ind) , data = vir[-i,]), vir)[i]
    }
    error <- mean((val-x[,dep])^2)
    Rcv <- 1-(mean((x[,ind]-mean(x[,ind]))^2))
    R <- summary(lm(paste0(dep, "~", ind) , data = x))$adj.r.squared
    return(data.frame(error,R,Rcv))
}