indices_sna <- function(lista.raster,index){
    sipi <- function(x){SIPI <- (resample(x$B08, x$B01, "bilinear")-x$B01)/(resample(x$B08, x$B01, "bilinear")-resample(x$B04, x$B01, "bilinear"));return(SIPI)}
    savi <- function(x){L <- 0.428;SAVI <- (x$B08-x$B04)/(x$B08+x$B04)*(1+L);return(SAVI)}
    pssr <- function(x){PSSR <- x$B08/x$B04;return(PSSR)}
    ndwi <- function(x){NDWI <- (resample(x$B08, x$B11, "bilinear")-x$B11)/(resample(x$B08, x$B11, "bilinear")+x$B11);return(NDWI)}
    ndvi <- function(x){NDVI <- (x$B08-x$B04)/(x$B08+x$B04);return(NDVI)}
    nbr <- function(x){NBR <- (resample(x$B08, x$B12, "bilinear")-x$B12)/(resample(x$B08, x$B12, "bilinear")+x$B12);return(NBR)}
    msi <- function(x){MSI <- x$B11/resample(x$B08, x$B11, "bilinear");return(MSI)}
    mcari <- function(x){MCARI <- ((x$B05-resample(x$B04,x$B05,"bilinear"))-0.2*(x$B05-resample(x$B03,x$B05,"bilinear")))*(x$B05/resample(x$B04,x$B05,"bilinear"));return(MCARI)}
    mari <- function(x){mARI <- ((1/resample(x$B03,x$B05,"bilinear"))-(1/x$B05))*x$B07;return(mARI)}
    gndvi <- function(x){GNDVI <- (x$B08-x$B03)/(x$B08+x$B03);return(GNDVI)}
    evi <- function(x){EVI <- 2.5*(x$B08-x$B04)/((x$B08+6*x$B04-7.5*x$B02)+1);return(EVI)}
    evi2 <- function(x){EVI2 <- 2.4*(x$B08-x$B04)/(x$B08+x$B04+1);return(EVI2)}
    chlred_edge <- function(x){CLR_ED <- (x$B07/x$B05)^(-1);return(CLR_ED)}
    arvi <- function(x){y <- 0.106;ARVI <- (x$B09-resample(x$B04,x$B09,"bilinear")-y-(resample(x$B04,x$B09,"bilinear")-resample(x$B02,x$B09,"bilinear")))/(x$B09+resample(x$B04,x$B09,"bilinear")-y-(resample(x$B04,x$B09,"bilinear")-resample(x$B02,x$B09,"bilinear")));return(ARVI)}
    ari1 <- function(x){ARI1 <- 1/resample(x$B03,x$B05,"bilinear")-1/x$B05;return(ARI1)}
    
    funs <- c(ari1,arvi,chlred_edge,evi,evi2,gndvi,mari,mcari,msi,nbr,ndvi,ndwi,pssr,savi,sipi)
    names(funs) <- c("ari1","arvi","chlred_edge","evi","evi2","gndvi","mari","mcari","msi","nbr","ndvi","ndwi","pssr","savi","sipi")
    
    if(is.list(lista.raster)==T & index == "all"){
        require(raster)
        require(doParallel)
        require(foreach)
        
        registerDoParallel(round(detectCores()*0.5))
        
        indices <- foreach(i=seq(funs),.packages = "raster") %dopar% {
            funs[[i]](lista.raster)
        }
        #indices <- lapply(funs, function(f) f(lista.raster))
        names(indices) <- names(funs)
        return(indices)
    }else{
        funs[[index]](lista.raster)
    }
}
