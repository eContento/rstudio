setwd("/lab/adictosaltrabajo/RStudio/01")

#leemos el CSV, como esta separado por ; y no por , lo indicamos
data <- read.csv("./monumentos-ciudad-madrid.csv", header = T, sep = ";")
data <- read.csv("./monumentos-ciudad-madrid.csv", header = T, sep = ";", stringsAsFactors = F)

#echamos un vistazo a las 5 primeras filas
head(data)

#no queremos tantas columnas, así que creamos un nuevo dataFrame con todas las filas y
# solo algunas columnas definidas en un conjunto mediante c("nombreCol1", "col2",...)
monumentos <- data[,c("PK","NOMBRE","FECHA","BARRIO","DISTRITO","LATITUD","LONGITUD")]

#también se podría haber hecho por IDs de las columnas 
#en R los vectores, arrays y matrices empiezan en 1
monumentos2 <- data[ , c(1,2,5,20,21,24,25)]

#vemos que son iguales
monumentos == monumentos2

#obtenemos el dataFrame con las columnas complementarias
monumentos3 <- data[ , -c(1,2,5,20,21,24,25)]

#nos damos cuenta que hay NAs y los quitamos
monumentos <- na.omit(monumentos)

#también quitamos las filas donde BARRIO o DISTRITO son "blancos"
monumentos <- monumentos[monumentos$BARRIO != "" & monumentos$DISTRITO != "" , ]

#Y nos quedamos con aquellas filas que son exactas
monumentos <- monumentos[nchar(monumentos$FECHA)==4,]

#Y los distritos los categorizamos como factores (hay 26 categorías)
monumentos$DISTRITO <- factor(monumentos$DISTRITO)

View(monumentos)

#limpiamos variables
data <- NULL
monumentos2 <- NULL
monumentos3 <- NULL

# Grabamos el objeto monumentos a un fichero de R
saveRDS(monumentos, file = "monumentos.rds")

# Leemos el objeto y lo cargarmos en memoria
monumentos <- readRDS(file = "./monumentos.rds")




