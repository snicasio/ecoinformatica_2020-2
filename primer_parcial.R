hill_sna <- function(x){
    qx <- data.frame(q0=0,q1=1,q2=2)
    hill_sna2 <- function(x,q){
        
        gamma <- function(x,q){
            x <- as.matrix(x/rowSums(x))
            if(q != 1){
                gam <- sum(colMeans(x)^q)^(1/(1-q))
            }else{
                gam <- exp(-(sum(colMeans(x)*log(colMeans(x)))))
            }
            return(gam)
        }
        
        alpha <- function(x,q){
            x <- as.matrix(x/rowSums(x))
            am0 <- ifelse(x == 0, 0, x^0)
            am1 <- x*log(x);am1[is.nan(am1)==T] <- 0
            if(q == 0){
                alp <- sum(colMeans(am0))^(1/(1-q))
            }else{
                if(q ==1){
                    alp <- exp(-mean(rowSums(am1)))
                }else{
                    if(q ==2){
                        alp <- sum(colMeans(x^q))^(1/(1-q))
                    }
                }
            }
            return(alp)
        }
        
        tab <- data.frame(alpha=alpha(x,q),beta=gamma(x,q)/alpha(x,q),gamma=gamma(x,q))
        return(tab)
    }
    
    return(do.call(rbind,lapply(qx,function(y) hill_sna2(x,y))))
}


word_fun_sna <- function(x){
    if(is.character(x)==T){
        word_pos <- match(unlist(strsplit(x,"")), letters)
    }else{
        stop("El vector no es de caracteres")
    }
    return(word_pos)
}


anagram_sna <- function(x,y){
    if(is.character(x)==T & is.character(y)==T){
        x_split <- unlist(strsplit(x,""))
        y_split <- unlist(strsplit(y,""))
        
        if(length(x_split) == length(y_split) & any(sort(x_split) %in% sort(y_split) == FALSE) == FALSE){
            return(TRUE)
        }else{
            return(FALSE)
        }
    }else{
        stop("El vector no es de caracteres")
    }
}


est_sna <- function(x){
    if(is.integer(x)==T | is.double(x)==T){
        y <- 0
        for(i in seq(x)-1){
            y <- y+x[i+1]
        }
        mn_x <- y/length(x)
        sd_x <- sqrt(sum((x-mn_x)^2)/(length(x)-1))
        st_x <- (x-mn_x)/sd_x
        return(st_x)
    }else{
        stop("El vector 'x' no es numerico")
    }
}


sum_acc_sna <- function(x){
    if(is.integer(x)==T | is.double(x)==T){
        y <- x[1]
        for(i in 2:length(x)){
            y[i] <- y[i-1]+x[i]
        }
        return(y)
    }else{
        stop("El vector 'x' no es numerico")
    }
}


fct_sna <- function(x){
    if(length(x)==1){
        x<-as.integer(x)
        y <- 1
        for(i in 1:x){
            y <- y*i
        }
        return(y)
    }else{
        stop("Solo acepto un valor en 'x'")
    }
}


skr_sna <- function(x){
    if(is.integer(x)==T | is.double(x)==T){
        y <- 0
        for(i in seq(x)-1){
            y <- y+x[i+1]
        }
        mn_x <- y/length(x)
        sd_x <- sqrt(sum((x-mn_x)^2)/(length(x)-1))
        skewness <- 1/length(x)*sum((x-mn_x)^3)/sd_x^3
        kurtosis <- 1/length(x)*sum((x-mn_x)^4)/sd_x^4
        return(data.frame(skewness,kurtosis))
    }
}