int_nic <- function(x,p){
    if(is.numeric(x)==T | is.integer(x)==T){
        N <- length(x)
        mn <- sum(x)/N
        vr <- sum((x-mn)^2)/(N-1)
        std <- sqrt(vr)
        se <- std/sqrt(N)
        Z <- qnorm((1-p)/2,lower.tail = F)
        EE <- se*Z
        li <- mn-EE
        ls <- mn+EE

        return(data.frame(li,mn,ls))
    }else{
        stop("Estas mal animal")
    }
}
