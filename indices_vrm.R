library(doParallel)
library(doSNOW)
library(foreach)
library(rgeos)
library(rgdal)
library(raster)

registerDoParallel(cores = 3)

Band.names <- read.csv("Band_names.csv")
Corte <- readOGR("Corte.shp")

Img <- lapply(list.files(pattern = "jp2"), raster::raster)
names(Img) <- as.vector(sapply(list.files(pattern = "jp2"), function(x) gsub(".jp2","",unlist(strsplit(x, "_"))[3])))



indices_vrm <- list()
for(i in seq(Img)){
    
}

img_more <- foreach(i=seq(Img), .packages = "raster") %dopar% {
    crop(Img[[i]], Corte)
}


resample(x, Img$B01, "bilinear") #para transformar hacia 60.
SIPI <- 
    ARVI

resample(x, Img$B05, "bilinear") #para transformar hacia 20.
NDWI
NDII
NBR-RAW
MSI
MCARI
mARI
CHL-RED-EDGE
ARI1

#no es necesario transformar.
SAVI
PSSR
NDVI
GNDVI
EVI
EVI2

Img$B01 #60
Img$B02 #10
Img$B03 #10
Img$B04 #10
Img$B05 #20
Img$B06 #20
Img$B07 #20
Img$B08 #10
Img$B08A #20
Img$B09 #60
Img$B10 #60
Img$B11 #20
Img$B12 #20