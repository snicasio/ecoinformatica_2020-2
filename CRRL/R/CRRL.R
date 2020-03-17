Hill <- function(x)
    {
    quest <- data.frame(q0=0,q1=1,q2=2)
    Hills2 <- function(x,q){
        
      
          gamma <- function(x,q)
            {
            x <- as.matrix(x/rowSums(x))
            if(q != 1){
                GM <- sum(colMeans(x)^q)^(1/(1-q))
            }else{
                GM <- exp(-(sum(colMeans(x)*log(colMeans(x)))))
            }
            return(GM)
        }
        
        alfa <- function(x,q)
            {
            x <- as.matrix(x/rowSums(x))
            sire <- ifelse(x == 0, 0, x^0)
            ranc <- x*log(x);ranc[is.nan(ranc)==T] <- 0
            if(q == 0){
                bar <- sum(colMeans(sire))^(1/(1-q))
            }else{
                if(q ==1){
                    bar <- exp(-mean(rowSums(ranc)))
                }else{
                    if(q ==2){
                        bar <- sum(colMeans(x^q))^(1/(1-q))
                    }
                }
            }
            return(bar)
        }
        
        tab <- data.frame(alfa=alfa(x,q),beta=gamma(x,q)/alfa(x,q),gamma=gamma(x,q))
        return(tab)
    }
    
    return(do.call(rbind,lapply(quest,function(y) Hills2(x,y))))
}




Sumac =
    function(x)
    {
        for(i in 2:length(x))  
            x[i] = x[i-1] + x[i]
        
        x 
    }
