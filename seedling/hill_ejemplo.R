
sd <- lapply(list.files()[grep(".csv",list.files())], read.csv)
    for(i in seq(sd)){
        sd[[i]] <- sd[[i]][,-1]
        sd[[i]] <- sd[[i]][!rowSums(sd[[i]])==0,]
    }

todo <- do.call(rbind,lapply(sd,colSums))

ab_rel <- lapply(sd, function(x) x/rowSums(x))  #   Abundancias relativas

(sum(ab_rel[[1]][,"Trophis.racemosa"]^2)+sum(ab_rel[[2]][,"Trophis.racemosa"]^2)+sum(ab_rel[[3]][,"Trophis.racemosa"]^2))/3
