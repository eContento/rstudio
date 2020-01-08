#seteamos el directorio de trabajo
setwd("/lab/adictosaltrabajo/rstudio/05")

# Vamos a leer un CSV del proyecto GeriaTIC
# https://www.geriatic.udc.es/el-proyecto/datos-abiertos-open-data-del-proyecto-geria-tic/
user_activity <- read.csv("./user_activity.csv", stringsAsFactors = F)

#para convertir string a fechas
user_activity$registerStartDate <- as.Date(user_activity$registerStartDate, format="%Y/%m/%d")

#nos quedamos con una version simplificada
activity <- user_activity[, c("userId","registerStartDate","steps","distance","calories")]

#nos fijamos en en usuario andarin
activity_user_10041 <- activity[activity$userId == 10041,]

# dividimos el año de muestreo en cuartos
activity_user_10041$quarters <- cut(activity_user_10041$registerStartDate, breaks = 4, labels= c("Q1", "Q2", "Q3", "Q4"))

View(activity_user_10041)

install.packages("beanplot")
library(beanplot)

beanplot(activity_user_10041$steps ~ activity_user_10041$quarters, col = c("red","yellow","green"), border=NA)

# Histogramas con funciones de densidad

#ahora vamos a fijarnos en el resto de ancianos que no son en 10041
demas_ancianos <- activity[activity$userId != 10041,]

# y dividimos el muestreo por las estacioens del año
breakpoints <- c(as.Date('2017-01-01'), as.Date('2017-12-21'), as.Date('2018-03-21'), as.Date('2018-06-21'), as.Date('2018-09-21'), as.Date('2018-12-31'))  

demas_ancianos$seasons <- cut(
    demas_ancianos$registerStartDate, 
    breaks = breakpoints,
    labels = c('Otoño', 'Invierno' , 'Primavera' , 'Verano', 'Otoño')
  )

#queremos ver si hay alguna relación entre el número de pasos y la estación del año
hist(demas_ancianos$steps, freq = F, xlab = "Pasos dados durante el año", ylab = NA, main = "Demás ancianos", breaks = 10 , density = 21)

invierno  <- demas_ancianos[demas_ancianos$seasons == "Invierno","steps"]
lines(density(invierno), col = "blue")

primavera <- demas_ancianos[demas_ancianos$seasons == "Primavera","steps"]
lines(density(primavera), col = "green")

verano    <- demas_ancianos[demas_ancianos$seasons == "Verano","steps"]
lines(density(verano), col = "yellow")

otoño     <- demas_ancianos[demas_ancianos$seasons == "Otoño","steps"]
lines(density(otoño), col = "orange")



# Matriz de ScatterPlots

#vemos como estan relacionadas las principales variables
pairs(demas_ancianos[,c("steps","distance","calories")], pch = 19)

#colores para otoño, invierno, primavera, verano
colores <- c("#9F5020", "#20379F", "#209F37", "#EAD332") 
pairs(demas_ancianos[,c("steps","distance","calories")], pch = 19, cex = 0.5,
      col = colores[demas_ancianos$seasons])


# Mapas

# leemos los precios que vimos en el ejemplo 02
precios <- readRDS("./precios.rds")

preciosL10 <- precios[precios$line == "L10",]

#creamos la columna con el rendimiento anualizado
preciosL10$rendimiento <- (12 * preciosL10$rental)/preciosL10$sale

# creamos un factor con la rentabilidad de 1 a 5 donde 1 es la mas baja y 5 la mas alta
preciosL10$rentabilidad <- cut(preciosL10$rendimiento, breaks = 5, labels = c(1,2,3,4,5))

# lo convertimos a número
preciosL10$rentabilidad <- as.integer(preciosL10$rentabilidad)


# instalamos y cargamos las librerías de Open Street Map
install.packages("osmdata")
library(osmdata)

# y la librería de GGPlot2 para pintar mapas
install.packages("ggmap")
library(ggmap)

#obtenemos una caja cuadrada con los limites de Madrid
bbox <- getbb("Madrid")

# obtenemos el mapa, indicando que la fuente es el OpenStreetMap
madrid.map <- get_map(bbox, source="osm")

# y lo pintamos con la librería GGPlot2
ggmap(madrid.map)


# ahora vamos a usar las coordenadas de las estaciones de Metro
bbox[1] <- min(preciosL10$lng) - 0.1
bbox[2] <- min(preciosL10$lat) - 0.01
bbox[3] <- max(preciosL10$lng) + 0.1
bbox[4] <- max(preciosL10$lat) + 0.01
madrid.map <- get_map(bbox, source="osm") 

# y pintamos las estaciones, pero más grandes aquellas que tienen mayor rentabilidad
ggmap(madrid.map) + 
  ggtitle("Mapa de rentabilidad de pisos para alquiler de Linea 10") +
  geom_point(data=preciosL10, 
             aes(x=lng,y=lat), 
             color="DB9000",
             fill="#DB9000",
             size= preciosL10$rentabilidad * 3, 
             shape=21,
             alpha= 0.7) +
  labs(x = "Longitud", y = "Latitud")





