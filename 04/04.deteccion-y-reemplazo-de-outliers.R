#seteamos el directorio de trabajo
setwd("/lab/adictosaltrabajo/rstudio/04")

# Vamos a leer un CSV del proyecto GeriaTIC
# https://www.geriatic.udc.es/el-proyecto/datos-abiertos-open-data-del-proyecto-geria-tic/
user_activity <- read.csv("./user_activity.csv")

user_activity <- read.csv("./user_activity.csv", stringsAsFactors = F)

#para convertir string a fechas
user_activity$registerStartDate <- as.Date(user_activity$registerStartDate, format="%Y/%m/%d")
user_activity$registerFinalDate <- as.Date(user_activity$registerFinalDate, format="%Y/%m/%d")

#para convertir strings a fecha-hora
user_activity$sleepStartDate <- strptime(user_activity$sleepStartDate, format="%Y/%m/%d %H:%M:%S", tz = "UTC")
user_activity$sleepStopDate <- strptime(user_activity$sleepStopDate, format="%Y/%m/%d %H:%M:%S", tz = "UTC")

#nos quedamos con una version simplificada
activity <- user_activity[,c("userId","registerStartDate","steps","distance","calories")]

# Histograma numero de pasos
hist(activity$steps, main="Frecuencia de número de pasos")

# hasta 2000 pasos es bajo
# de 2000 - 4000 pasos sería medio
# de 4000 - 6000 pasos estaría alto
# > 6000 muy alto

# Vamos a convertir la variable numerica "pasos" en categorica
# para ello definimos los puntos de corte
breakPoints <- c(0, 2000, 4000, 6000, Inf)
categories <- c("Low", "Medium", "High", "Very High")

# y cortamos la variable número de pasos segun esta categorizacion
activity$steps.F <- cut(activity$steps, breaks = breakPoints, labels = categories)

summary(activity$steps)

# fijemonos que la media (1687.3) y la mediana (878.5) estan muy distantes
# y guardemos el valor del primer y tercer cuartil en dos variables
Q1 <- 353.0
Q3 <- 1947.5

# Se define el rango intercuantílico como:
IQR <-  Q3 - Q1

# IQR es 1594.5 pasos

# Se define como valor atípico leve aquel que dista 1,5 veces 
# el rango intercuantílico por debajo de Q1 o por encima de Q3 
# q < Q1 - 1,5 IQR o bien q > Q1 + 1,5 IQR
# y valor atípico extremo aquel que dista 3 veces el rango 
# intercantílico por debajo de Q1 o por encima de Q3
#  q < Q1 - 3 IQR o bien q > Q1 + 3 IQR

# vamos a calcular cuales serían los umbrales
umbral_sup <- Q3 + (1.5 * IQR)  # 4339.25 pasos
umbral_inf <- Q1 - (1.5 * IQR)  # -2038.75

# quitamos los outliers superiores (inferiores no hay)
steps <- activity[activity$steps < 4339.25, c("steps")]

summary(steps)

# nos fijamos en el userId 10039

activity_10039 <- activity[activity$userId == 10039,]

summary(activity_10039$steps)

steps_10039 <- activity_10039$steps

# dibujamos el grafico de caja y bigotes
boxplot(steps_10039)

# guardamos los outliers en una variable
outliers_10039 <- boxplot(steps_10039)$out

# veo que el usuario 10041 es el que más outliers
# lo quito

activity <- activity[activity$userId != 10041,]

boxplot(activity$steps ~ activity$userId)

# este boxplot sería equivalente al anterior
boxplot(steps ~ userId, 
        data = activity,
        main = "Pasos por usuario")

outliersReplace <- function(data, lowLimit, highLimit){
  data[data<lowLimit] <- mean(data)
  data[data>highLimit] <- median(data)
  data
}

steps_10039_2 <- outliersReplace(steps_10039, 1045, 4884.5)

summary(steps_10039)
summary(steps_10039_2)

par(mfrow = c(1,2))

boxplot(steps_10039, main = "Sin reemplazo de outliers")
boxplot(steps_10039_2, main = "Con reemplazo de outliers")

# df es el dataFrame que recibimos (ej. activity)
# colNameData es la columna de los datos (ej. "steps")
# colNameBy es la columna por la que trocearemos (ej. "userId")
outliersReplace <- function(df, colNameData, colNameBy){
  #creamos una nueva columna llamada igual que colNameData pero con .R
  colNameData.R <- paste(colNameData, "R", sep=".")
  df[colNameData.R] <- df[colNameData]
  
  #obtenemos los IDs por los que partir el dataframe
  IDs <- unique(df[,c(colNameBy)])
  for (id in IDs){
    data <- df[df[colNameBy] == id, c(colNameData) ]
    
    Q  <- quantile(data)
    minimo <- Q[1]    # valor minimo
    Q1     <- Q[2]    # primer cuartil
    Me     <- Q[3]    # mediana
    Q3     <- Q[4]    # tercer cuartil
    maximo <- Q[5]    # valor maximo
    IQR <- Q3 - Q1
    
    lowLimit <- max(minimo, Q1 - 1.5*IQR)
    highLimit <- min(maximo, Q3 + 1.5*IQR)
    
    # todos los valores donde colNameBy es igual a id
    # y el valor de colNameData es > Q3 + 1.5 * IQR
    # lo reemplazamos por la mediana
    df[df[colNameBy] == id & df[colNameData] > highLimit, c(colNameData.R)] <- Me
    
    # lo mismo para el umbral inferior
    df[df[colNameBy] == id & df[colNameData]  < lowLimit, c(colNameData.R)] <- Me
    
    cat(paste("El", colNameBy, id, "la mediana(", colNameData, ") ==", Me, "\n", sep=" " ))
    
  }
  df
}

activity <- outliersReplace(activity,"steps","userId")

par(mfrow = c(2,1))

boxplot(steps   ~ userId, data = activity, main = "Sin reemplazo")
boxplot(steps.R ~ userId, data = activity, main = "Con reemplazo")
