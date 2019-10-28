#seteamos el directorio de trabajo
setwd("/lab/adictosaltrabajo/rstudio/03")

# ---------------------------------------------------------- 
# Construir DataFrames
# ---------------------------------------------------------- 

# Ciudades con más 150.000 habitantes en Madrid.

nombre <- c("Madrid","Móstoles","Alcalá de Henares","Fuenlabrada","Leganés","Getafe","Alcorcón")
poblacion <- c(3265038,205015,203686,198560,186552,170115,168523)
superficie <- c(605.77, 45.36, 87.72, 39.41, 43.09, 78.38, 33.73)
altitud <- c(657, 660, 587, 662, 667, 622, 711)


municipios <- data.frame(nombre, poblacion, superficie, altitud)


# ---------------------------------------------------------- 
# LEER FICHERO DE ANCHO FIJO                                 
# ---------------------------------------------------------- 

# se ve cláramente que tiene 5 columnas 
# y que las 4 primeras filas las podemos saltar
# y añadimos los nombres de las columnas
data <- read.fwf("https://www.cpc.ncep.noaa.gov/data/indices/wksst8110.for",
         widths=c(15, 13, 13, 13, 8), 
         skip=4,
         col.names = c("Week", "Nino1+2", "Nino 3", "Nino 34", "Nino 4"))


# los valores contienen espacios delante y detrás.
# con el simbolo menos, indicamos el ancho, pero que no queremos la columna
# y además indicamos que los strings no son factores
data <- read.fwf("https://www.cpc.ncep.noaa.gov/data/indices/wksst8110.for",
                 widths=c(-1, 9, -5, 8, -5, 8, -5, 8, -5, 8), 
                 skip=4,
                 col.names = c("Week", "Nino1+2", "Nino3", "Nino34", "Nino4"),
                 stringsAsFactors = F
                 )



# ---------------------------------------------------------- 
# LEER HTML de una WEB                          
# ---------------------------------------------------------- 

install.packages("XML")
library(XML)
urlDeEjemplo <- "https://cincodias.elpais.com/mercados/bolsa/ibex_35/582/"
doc <- readHTMLTable(urlDeEjemplo)

# nos devuelve un error:
# Warning message: XML content does not seem to be XML:
# eso es porque readHTMLTable no puede leer HTTPS

# para eso usaremos la librería RCurl que ya vimos
install.packages("RCurl")
library(RCurl)

# y obtenemos el HTML y lo guardamos en una variable de R
htmlData <- getURL(urlDeEjemplo)

# ahora buscamos las tablas existentes en el HTML
tables <- readHTMLTable(htmlData)

# vemos que es una lista de dos dataFrames (hay dos tablas)
# nos quedamos con el que se llama indice_valores
ibex35 <- tables[["indice_valores"]]

# visualizamos los datos que hemos obtenido
View(ibex35)



# ---------------------------------------------------------- 
# LEER XML de una WEB                          
# ---------------------------------------------------------- 

# instalamos/actualizamos la librería y la cargamos
install.packages("XML")
library(XML)

# Vamos a leer un XML de la red
# Número de registros de autoridad en el catálogo de la BNE en 2018
# http://www.bne.es/media/datosgob/estadisticas/autoridades/Autoridades_2018.xml

urlBNE2018 <- "http://www.bne.es/media/datosgob/estadisticas/autoridades/Autoridades_2018.xml"
xmlDocument <- xmlParse(urlBNE2018)
rootNode <- xmlRoot(xmlDocument)

# vemos que rootNode es de tipo External pointer of class "XMLInternatElementNode"
# es decir, que contiene la lista de items. Veamos el item número 5
rootNode[5]

# se trata de un XML muy sencillo que no tiene atributos y sólo tiene valores
# Lo que queremos hacer es por cada ITEM extraer los pares clave-valor
# usamos xmlSApply que a cada elemento le aplica una función. 
catalogo <- xmlSApply(rootNode, function(x) {
    xmlSApply(x, xmlValue)
})

# la hemos aplicado dos veces. Vemos la pinta que tiene
View(catalogo)

# Vemos que las filas están cambiadas por columna.

# transponemos la matriz catalogo y la guardamos en la misma variable
catalogo <- t(catalogo)

# y lo convertimos en un dataFrame
catalogo <- data.frame(catalogo)

# pero los nombres de las filas son item.1 ... item.10.
# podemos solventarlo poniéndolos a NULL y será 1 ... 10
row.names(catalogo) <- NULL

# pero a lo mejor preferimos usar como rowNames el valor de la columna "Datos"
# y quitar dicha columna que no aporta información numérica
row.names(catalogo) <- catalogo$Datos
catalogo$Datos <- NULL



# ---------------------------------------------------------- 
# La sintaxis del $
# ---------------------------------------------------------- 

# ya la hemos usado sin darnos cuenta para acceder a valores de columnas
# por ejemplo, catalogo$Datos

# hay una web que muestra datos de Alcorcón
# https://www.epdata.es/datos/datos-graficos-estadisticas-municipio/52/alcorcon/641

install.packages("jsonlite")
library(jsonlite)
library(curl)
poblacion_alcorcon <- fromJSON("poblacion_alcorcon.json")

# Vemos que la variable es una "List of 5". Veamos que aspecto tiene la lista

# Al ser una lista podemos recorrerla como array asociativo
pa1 <- poblacion_alcorcon[["Datos"]][["Metricas"]][["Datos"]][[1]]

# pero podemos usar la potencia del operador $ (RStudio nos autocompleta)
pa2 <- poblacion_alcorcon$Datos$Metricas$Datos[[1]]

identical(pa1,pa2)



# ---------------------------------------------------------- 
# Primeros graficos en R
# ---------------------------------------------------------- 

# muchas veces nos harán falta dataSets para hacer pruebas 
# R viene con un conjunto de muestras de el package dataSets por defecto

# vamos a usar el dataSet airquality que nos dice la calidad del aire en NY
# de mayo a septiembre de 1973

View(airquality)

hist(airquality$Temp)

numsaltos <-  max(airquality$Temp) - min(airquality$Temp)

hist(airquality$Temp, breaks = numsaltos/2)

hist(airquality$Temp, 
    breaks = numsaltos/2,
    col = "lightgoldenrodyellow",
    border = "lightgoldenrod3",
    xlab = "Temperatura (ºF)",
    ylab = "Frecuencia",
    main = "Frecuencia de temperaturas en NY (1973)"
    )

# para conocer el nombre de los colores podemos usar
colors()












