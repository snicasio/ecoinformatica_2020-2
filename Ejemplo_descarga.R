

library(dataone)

cn <- CNode("PROD")
mn <- getMNode(cn, "urn:node:KNB")

mySearchTerms <- list(q="keywords:Plant+OR+plant",
                      fq="attribute:biomass+OR+Biomass+OR+npp+OR+NPP",
                      fq="formatType:METADATA",
                      fq="id:doi*",
                      fl="id,title,dateUploaded,abstract,size",
                      sort="dateUploaded+desc")

result <- query(mn, solrQuery = mySearchTerms, as="data.frame")




        #   Metodo de obtencion de datos: Descarga de paquete

getPackage(mn, id = result[6,"id"], dirPath = getwd())


        #   Metodo de obtencion de datos: Busqueda amplia

d1c <- D1Client("PROD", "urn:node:KNB")

result <- query(d1c@mn, solrQuery = mySearchTerms, as="data.frame", parse = F)

dataObj <- getDataObject(d1c, result[2,"id"])
Dat1 <- getData(dataObj)
metadataXML <- rawToChar(Dat1)


df <- read.csv(theData, stringsAsFactors=FALSE)
df[1,]







library(dataone)
