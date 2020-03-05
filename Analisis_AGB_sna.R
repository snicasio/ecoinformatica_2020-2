library("raster")
library("rgeos")
library("rgdal")


raster_names <- list.files()[grep(".tif",list.files())] #   Elaboramos un vector con los nombres de los archivos raster (terminacion *.tif)
raster_names <- raster_names[c(3,1,2,4:16)] #   Acomodamos la posicion de los nombres (para que "bio" quede al principio)

sample_plots <- readOGR("plots.shp")    #   Cargamos el shapefile de los plots de trabajo
raster_list <- lapply(raster_names,raster)  #   Cargamos los archivos raster en el orden en que se encuentran dentro de "raster_names"
    names(raster_list) <- sapply(raster_list,function(x) x@data@names)  #   Le ponemos nombre a cada elemento de la lista


ind_list <- lapply(raster_list,function(x) unlist(extract(x,sample_plots))) #   Extraemos el valor del pixel que hay dentro de cada plot
ind_tab <- data.frame(do.call(cbind,ind_list))    #   Convertimos la lista a un data.frame
rownames(ind_tab) <- seq(rownames(ind_tab))


        ##  SEGUNDO PASO: HACER LAS TRANSFORMACIONES NECESARIAS EN CADA COLUMNA PARA QUE SE CUMPLA CON LA DISTRIBUCION NORMAL

library("moments")

norm_tab <- data.frame(normal = sapply(ind_tab,function(x) shapiro.test(x)$p.value),
                       sesgo = sapply(ind_tab,skewness))
norm_tab$normal <- ifelse(norm_tab$normal <= 0.05, "NO", "SI")


ind_tab[,c("bio","chlred_edge","mcari","msi")] <- log(ind_tab[,c("bio","chlred_edge","mcari","msi")])
ind_tab[,c("ari1","arvi","evi2","mari","nbr","ndvi","ndwi","savi","sipi")] <- exp(ind_tab[,c("ari1","arvi","evi2","mari","nbr","ndvi","ndwi","savi","sipi")])

ind_tab <- ind_tab[,-c(2,8,16)]
head(ind_tab)

        ##  TERCER PASO: ELIMINAR LAS VARIABLES REDUNDANTES. OSEA, AQUELLAS QUE TENGAN UNA ALTA CORRELACION (r > 0.75)

cor_tab <- cor(ind_tab)
    cor_tab
    diag(cor_tab)
    diag(cor_tab) <- 0
    cor_tab <- data.frame(cor_tab)

num_row <- 1

while(num_row <= nrow(cor_tab)){
    cor_tab <- cor_tab[!cor_tab[,num_row] > 0.75,!cor_tab[,num_row] > 0.75]
    num_row <- num_row+1
}
cor_tab



        ##  CUARTO PASO: ELABORAR TODAS LAS COMBINACIONES POSIBLES DE MODELOS

modelo <- lm(bio~evi,data = ind_tab)

modelo
summary(modelo)
anova(modelo)

plot(bio~evi,data = ind_tab)
abline(modelo, lwd = 2, col = "red")
abline(h=mean(ind_tab$bio), lty = 2, lwd = 2, col = "darkgreen")




