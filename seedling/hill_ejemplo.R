
sd <- lapply(list.files()[grep(".csv",list.files())], read.csv)
    for(i in seq(sd)){
        sd[[i]] <- sd[[i]][,-1]
    }


do.call(rbind,lapply(sd,colSums))
