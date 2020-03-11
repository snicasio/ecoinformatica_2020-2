
sd <- lapply(list.files()[grep(".csv",list.files())], read.csv)
    for(i in seq(sd)){
        sd[[i]] <- sd[[i]][,-1]
        sd[[i]] <- sd[[i]][!rowSums(sd[[i]])==0,]
    }



todo <- do.call(rbind,lapply(sd,colSums))


sd[[1]]/rowSums(sd[[1]])
sd[[2]]/rowSums(sd[[2]])
