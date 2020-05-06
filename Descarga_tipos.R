

library(dataone)

cn <- CNode("PROD")
mn <- getMNode(cn, "urn:node:KNB")

mySearchTerms <- list(q="keywords:Interaction+OR+interaction",
                      fq="attribute:coordinates+OR+Coordinates",
                      fl="id,title,dateUploaded,abstract,size",
                      sort="dateUploaded+desc")

result <- query(mn, solrQuery = mySearchTerms, as="data.frame")

result$id
result$title



        #   Metodo de obtencion de datos: Descarga de paquete. Hay que establecer un directorio de trabajo previamente

getPackage(mn, id = result[1,"id"], dirPath = getwd())


        #   Metodo de obtencion de datos: Busqueda amplia

d1c <- D1Client("PROD", "urn:node:KNB")

result <- query(d1c@mn, solrQuery = mySearchTerms, as="data.frame", parse = F)

dataObj <- getDataObject(d1c, result[10,"id"])
Dat1 <- getData(dataObj)
metadataXML <- rawToChar(Dat1)

df <- read.csv(Dat1, stringsAsFactors=FALSE)


