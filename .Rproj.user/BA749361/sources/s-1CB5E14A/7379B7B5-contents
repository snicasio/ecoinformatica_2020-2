library(raster)
library(rgeos)
library(rgdal)


        ##  EJERCICIO: DETERMINAR LA CONTRIBUCION DE LAS VARIABLES CLIMATICAS Y ESPACIALES SOBRE EL TAMANO CORPORAL DE LOS MAMIFEROS TERRESTRES

            ### Base PanTHERIA (EJEMPLO CON MAMIFEROS NO VOLADORES Y NO ROEDORES)

Mam <- read.csv("PanTHERIA.csv", h = T)
    Mam[Mam == -999] <- NA
    Mam <- Mam[Mam$X12.2_Terrestriality == 2, ] #   Mamiferos terrestres
    Mam <- Mam[!Mam$MSW05_Order == "Chiroptera",]   #   Mamiferos no voladores
    Mam <- Mam[!Mam$MSW05_Order == "Rodentia",] #   Sin roedores
    Mam <- Mam[complete.cases(Mam$X26.4_GR_MidRangeLat_dd), ]   #   Especies georeferenciadas


            ### Extraccion de coordenadas y proyeccion para representarlas de manera espacial

Coordenadas <- Mam[, c(47,44)]
    Coordenadas <- SpatialPoints(Coordenadas, proj4string = CRS("+proj=longlat"))
    Coordenadas
    plot(Coordenadas)
    

            ### Descarga de capas bioclimaticas con una resolucion de 10 Km

Clim <- getData(name = "worldclim", var = "bio", res = 10)
    Clim


            ### Extraccion de valores bioclimaticos a traves de las coordenadas de cada especie

Tabla.clima <- data.frame(extract(Clim, Coordenadas))


            ### Tabla final que incluye la infomacion de los mamiferos, sus referencias espaciales y valores bioclimaticos

Mam.clim <- cbind(Mam, Tabla.clima)
    Mam.clim <- Mam.clim[complete.cases(Mam.clim$bio1), ]
    Mam.clim <- Mam.clim[complete.cases(Mam.clim$X5.1_AdultBodyMass_g), ]
    Mam.clim$X5.1_AdultBodyMass_g <- log(Mam.clim$X5.1_AdultBodyMass_g)


            ### Modelos que mejor explican la variacion del tamano corporal
            ### Nota: Esto tambien lo haran para mamiferos voladores y roedores (cada uno por separado)
            ### Quiero ver sus tablas AICc y el model average de las variables con mayor importancia

names(Mam.clim)

Mam.clim <- Mam.clim[ ,c(7,44,47,56:74)]
    names(Mam.clim)[1:3] <- c("Size", "Lat", "Lon")


mod.ej <- glm(Size ~ ., data=Mam.clim, family = "gaussian", na.action = "na.fail")
    mod.ej

library(MuMIn)

ej <- dredge(mod.ej, beta = "sd", m.lim = c(0,4))
    ej[1:5,]


        ##  MAPA

            ### Raster de los valores de latitud
    
    Lat <- Clim$bio1
    values(Lat) <- coordinates(Clim$bio1)[ ,2]
    names(Lat) <- "Lat"
    plot(Lat)

    
            ### Raster de los valores de longitud
    
    Lon <- Clim$bio1
    values(Lon) <- coordinates(Clim$bio1)[ ,1]
    names(Lon) <- "Lon"
    plot(Lon)
    
    
            ### Objeto "stack" para elaborar el mapa de disribucion de tamanos

Raster.var <- stack(Lat, Lon, Clim$bio8, Clim$bio19)   #   Nota: las capas que coloques en el stack puede cambiar entre modelos


            ### Elaboracion del mapa

x <- glm(Size ~ Lat+Lon+bio8+bio19, data = Mam.clim, family = "gaussian")
summary(x)


Mam.mass <- predict(Raster.var, x) #    La "x" representara su mejor modelo

Col.mio <- colorRampPalette(c("blue", "cyan", "yellow", "orange", "darkred"))   #   Paleta de colores
    
plot(Mam.mass, col = Col.mio(50))
    plot(Coordenadas, add = T)
    
