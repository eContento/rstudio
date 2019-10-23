#seteamos el directorio de trabajo
setwd("/lab/adictosaltrabajo/rstudio/02")

# como vamos a trabajar con JSON me instalo la librería
# y la cargo en memoria
install.packages("jsonlite")
library(jsonlite)

# vamos a trabajar con URLs en lugar de ficheros, así que cargamos la libreria
# y la cargamos en memoria
install.packages("curl")
library(curl)

uri_paradas_metro <- "https://idealista.carto.com/api/v2/sql?q=select ID, NAME, LINE, PLACE, LAT, LNG from public.paradas_metro_madrid"
paradas_metro <- fromJSON(uri_paradas_metro)

q <- curl_escape("select ID, NAME, LINE, PLACE, LAT, LNG from public.paradas_metro_madrid")
uri_paradas_metro <- paste("https://idealista.carto.com/api/v2/sql?q=",q, sep="")
paradas_metro <- fromJSON(uri_paradas_metro)

paradas_metro <- paradas_metro[["rows"]]

View(paradas_metro[paradas_metro$name == "Puerta del Sur",])


p <- curl_escape("select ID, SALE, RENTAL from public.precio_metro_201605")
uri_precios_estaciones <- paste("https://idealista.carto.com/api/v2/sql?q=",p, sep="")
precios_estaciones <- fromJSON(uri_precios_estaciones)

precios_estaciones <- precios_estaciones[["rows"]]

paradas_metro_precio <- merge(paradas_metro, precios_estaciones)




query <- paste("select ",
                  "pmm.ID, pmm.NAME, pmm.LINE, pmm.PLACE, pmm.LAT, pmm.LNG, pr.SALE, pr.RENTAL ",
               "from ",
                  "public.paradas_metro_madrid pmm ",
                  "inner join public.precio_metro_201605 pr ",
                  "  on pmm.id = pr.id ",
                "order by pmm.ID",
                sep="")
uri <- paste("https://idealista.carto.com/api/v2/sql?q=",curl_escape(query), sep="")
precios <- fromJSON(uri)
precios <- precios[["rows"]]


identical(paradas_metro_precio,precios)

precios$line <- as.factor(precios$line)  

saveRDS(precios, file = "precios.rds")

# vamos a limpiar todas las variables y nos quedamos solo con el dataframe de precios

#creamos la columna con el rendimiento anualizado
precios$rendimiento <- (12 * precios$rental)/precios$sale

#creamos otra columna con los beneficios anuales brutos por cada 100.000 euros invertidos
precios$beneficio100k <- round(precios$rendimiento * 100000,0)

#restringir sólo a la linea 10
precios_linea10 <- precios[precios$line == "L10",]

