##
O <- function(x,y) {
    if(res(x) == res(y)){
        return(
            PSSRb1 <-(Img$B08 / Img$B04),
            L = 0.428,
            SAVI<- (Img$B08 - Img$B04) / (Img$B08 + Img$B04 + L) * (1.0 + L),
            NDVI<- (Img$B08 - Img$B04) / (Img$B08 + Img$B04),
            GNDVI<- (Img$B08 - Img$B03) / (Img$B08 + Img$B03),
            EVI2<-2.4 * (Img$B08 - Img$B04) / (Img$B08 + Img$B04 + 1.0),
            CHL<- math.pow((Img$B07 / Img$B05), (-1.0)) )
    }else{
        if(res(x) < res(y))
        {resample(x,y, "bilinear")
            return(NDWI <- (Img$B08 - Img$B11)/(Img$B08+ Img$B11), #
                   NDII<-(Img$B08 - Img$B11) / (Img$B08 + Img$B11),#
                   NBR<-(Img$B08 - Img$B12) / (Img$B08 + Img$B12),#
                   #
                   ARI<- 1.0 / Img$B03 - 1.0 / Img$B05)
            
        }else{
            if(res(x) < res(y))
            {resample(x,y, "bilinear")
            }
        }
    }
}

O(Img)

p<- function(x, y)
    if(res(x) < res(y))
    {resample(x,y, "bilinear", filename="x")}
L = 0.428 

indices_arz<- list(
    PSSRb1<-(Img$B08 / Img$B04),
    SAVI<- (Img$B08 - Img$B04) / (Img$B08 + Img$B04 + L) * (1.0 + L),
    NDVI<- (Img$B08 - Img$B04) / (Img$B08 + Img$B04),
    GNDVI<- (Img$B08 - Img$B03) / (Img$B08 + Img$B03),
    EVI2<-2.4 * (Img$B08 - Img$B04) / (Img$B08 + Img$B04 + 1.0) )

names(indices_arz)<- c(PSSRb1,SAVI, NDVI, GNDVI, EVI2)


CHL<- math.pow((Img$B07 / Img$B05), (-1.0)) 
NDWI <- (Img$B08 - Img$B11)/(Img$B08+ Img$B11) #
NDII<-(Img$B08 - Img$B11) / (Img$B08 + Img$B11)#
NBR<-(Img$B08 - Img$B12) / (Img$B08 + Img$B12)#
MSI<- Img$B11 / Img$B08 #
ARI<- 1.0 / Img$B03 - 1.0 / Img$B05

SIPI1<-(Img$B08-Img$B01) / (Img$B08 - Img$B04) #
MCARI<-((Img$B05 - Img$B04) - 0.2 * (Img$B05 - Img$B03)) * (Img$B05 / Img$B04)#
MARI<- ((1.0 / Img$B03) - (1.0 / Img$B05)) * Img$B07#
y = 0.106
ARVI<-(Img$B09 - Img$B04 - y * (Img$B04 - Img$B02)) / (Img$B09 + Img$B04 - y * (Img$B04 - Img$B02))#
EVI<-  2.5 * (Img$B08 - Img$B04) / ((Img$B08 + 6.0 * Img$B04 - 7.5 * Img$B02) + 1.0)



