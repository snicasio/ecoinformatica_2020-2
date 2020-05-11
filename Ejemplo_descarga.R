

library(dataone)


    #   Ejemplo con datos descargables

cn <- CNode("PROD")
mn <- getMNode(cn, "urn:node:KNB")
mySearchTerms <- list(q="abstract:salmon+AND+keywords:acoustics+AND+keywords:\"Oncorhynchus nerka\"",
                      fl="id,title,dateUploaded,abstract,size",
                      fq="dateUploaded:[2013-01-01T00:00:00.000Z TO 2014-01-01T00:00:00.000Z]",
                      sort="dateUploaded+desc")
result <- query(mn, solrQuery=mySearchTerms, as="data.frame")
result[1,c("id", "title")]
pid <- result[1,'id']   #   El DOI es fundamental para acceder a los datos


        ##   Metadatos

library(XML)
metadata <- rawToChar(getObject(mn, pid))


        ##   Descarga de archivo

dataRaw <- getObject(mn, "df35d.443.1") # Lo que viene entre comillas es el DOI. En este ejemplo cambio el DOI
dataChar <- rawToChar(dataRaw)
theData <- textConnection(dataChar)
df <- read.csv(theData, stringsAsFactors=FALSE)
df[1,]







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
