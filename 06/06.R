#seteamos el directorio de trabajo
setwd("/lab/adictosaltrabajo/rstudio/06")

# leemos los precios que vimos en el ejemplo 02
precios <- readRDS("./precios.rds")

#creamos la columna con el rendimiento anualizado
precios$rendimiento <- (12 * precios$rental)/precios$sale

# creamos un factor con la rentabilidad
precios$rentabilidad <- cut(precios$rendimiento, 
                            breaks = 5, 
                            labels = c("Muy Bajo","Bajo","Medio","Alto","Muy Alto"))

View(precios)



install.packages("caret")
library(caret)

filas.entrenamiento <- createDataPartition(precios$rentabilidad, p = 0.7, list = FALSE)

#la variable filas.entrenamiento tiene los IDs de las filas seleccionadas.

#El conjuto de entrenamiento son esas filas y todas las columnas
conjuntoEntrenamiento <- precios[filas.entrenamiento,]

#Y el conjunto de test son las filas restantes, y todas las columnas
conjuntoTest <- precios[-filas.entrenamiento,]


install.packages("rpart")
library(rpart)

# vamos a hacer un modelo de clasificación basado el árboles de decisión
# estudiando la rentabilidad en base a la localizacion geografica (latitud, longitud)
modeloArbolDecicion <- rpart(
            rentabilidad ~ lat + lng,
            data = precios[filas.entrenamiento,],
            method = "class")

modeloArbolDecicion


#pintamos el arbol de decision
install.packages("rpart.plot")
library(rpart.plot)
prp(modeloArbolDecicion, type = 2, extra = 102)

#hacemos una predicción sobre el conjunto de test
prediccionArbolDecicion <- predict(modeloArbolDecicion, precios[-filas.entrenamiento,], type="class")

#añadimos la predicción en una columna nueva
precios[-filas.entrenamiento,c("P(arbol_decision)")] <- prediccionArbolDecicion
  
#como de bueno es este modelo

rentabilidad <- precios[-filas.entrenamiento,c("rentabilidad")]
matrizDeConfusionArbolDecicion <- table(rentabilidad, 
                                        prediccionArbolDecicion, 
                                        dnn = c("retabilidad","rentabilidad predicha"))

#clasificacion de aciertos vs fallos
matrizDeConfusionArbolDecicion

#probabilidad de cada caso
prop.table(matrizDeConfusionArbolDecicion)

#probabilidad por COLUMNAS en porcentaje
round(100 * prop.table(matrizDeConfusionArbolDecicion,2))

#barplot con la probabilidad
barplot(round(100 * prop.table(matrizDeConfusionArbolDecicion,1)), 
        legend = T, args.legend=list(x=5.1,y=160,bty = "n"))




#veamos otros algoritmos de clasificacion: randomForest
install.packages("randomForest")
library(randomForest)


modeloRandomForest <- randomForest(x = precios[filas.entrenamiento,c("lat","lng")],
                                   y = precios[filas.entrenamiento,c("rentabilidad")], 
                                   ntree = 200)

prediccionRandomForest <- predict(modeloRandomForest, precios[-filas.entrenamiento,], type="class")

#añadimos la predicción en una columna nueva
precios[-filas.entrenamiento,c("P(bosque_aleatorio)")] <- prediccionRandomForest




matrizDeConfusionRandomForest <- table(rentabilidad, 
                                       prediccionRandomForest, 
                                       dnn = c("retabilidad","rentabilidad predicha"))

#clasificacion de aciertos vs fallos
matrizDeConfusionRandomForest

#probabilidad de cada caso
prop.table(matrizDeConfusionRandomForest)

#probabilidad por COLUMNAS en porcentaje
round(100 * prop.table(matrizDeConfusionRandomForest,2))



######################
# Curvas ROC         #
######################
# creamos un factor con la rentabilidad
precios$inversion <- cut(precios$rendimiento, 
                            breaks = 5, 
                            labels = c("NO","NO","NO","SI","SI"))

# creamos un modelo de Bosque Aleatorio con el conjunto de entrenamiento
modRF <- randomForest(x = precios[filas.entrenamiento,c("lat","lng")],
                                   y = precios[filas.entrenamiento,c("inversion")], 
                                   ntree = 200)

# hacemos una predicción sobre el conjunto de test
predRF <- predict(modRF, precios[-filas.entrenamiento,], type="class")
precios[-filas.entrenamiento,c("predRF")] <- predRF

# visualizamos los resultados
View(precios[-filas.entrenamiento,])

inversion <- precios[-filas.entrenamiento, c("inversion")]
matrizDeConfusionRF <- table(inversion, 
                             predRF, 
                             dnn = c("inversion","pred"))

#clasificacion de aciertos vs fallos
matrizDeConfusionRF




# hacemos una predicción pero en lugar de class que nos de la probabilidad
# predRF <- predict(modRF, precios[-filas.entrenamiento,], type="class")
  probRF <- predict(modRF, precios[-filas.entrenamiento,], type="prob")


    
install.packages("pROC")
library(pROC)

#comparamos el valor original con la probabilidad del SI  
roc.objRF <- roc( inversion, probRF[,2], auc = TRUE, ci = TRUE  )

plot(roc.objRF)
  
roc.objRF$auc