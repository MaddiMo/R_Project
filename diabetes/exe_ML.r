
install.packages("DataExplorer", dependencies = T)
install.packages("SmartEDA", dependencies = T)

# TAREAS A HACER
#   1. EDA
#   2. numericas
#     a) no numericas : dummies
#     b) numericas: 
#       1. outliers
#       2. 0(2) != 0 // crear nueva tabla, quitar outliers
#       2. correlacion 
#       3. normalizacion # normalizacion: para que las variables numericas esten en la misma escala

# Cargamos datos

library(dplyr)

df = read.csv("diabetes_prediction_dataset.csv", sep=",")

library(DataExplorer)
create_report(df)

# Divido los datos en categóricos y no categóricos
numericas = sapply(df, is.numeric)
numericas = df [,numericas]
noNumericas = sapply(df, is.numeric)==F
noNumericas = df[, noNumericas]

# quitar variables "diabetes" ya que es la variable objetivo

numericas$diabetes = NULL
diabetes = df$diabetes
df$diabetes = NULL

FindOutliers = function(numericas) {
  lowerq = quantile(numericas, 0.25)
  upperq = quantile(numericas, 0.75)
  iqr = upperq - lowerq   #rango intercuartílico (IQR)
  extreme.upper = (iqr * 3) + upperq
  extreme.lower = lowerq - (iqr * 3)
  result = numericas > extreme.upper | numericas < extreme.lower
  return(result)
} # analisis univariante

i = 1
cont = as.data.frame(i:nrow(numericas))
while (i <= ncol(numericas)) {
  temp_2 = FindOutliers(numericas[[i]]) # vector logico de la fila que cumplen 
    # esa condicion (internamente hace un "for")
  cont = cbind.data.frame(cont,temp_2) # join a nivel de columnas
  i=i+1
}
cont[[1]] = NULL

j = 1
new = as.data.frame(df[1,]) # simplemente instrumental (para el esqueleto del nuevo dataframe)
while (j <= nrow(cont)) {
    if ((rowSums(cont)[j] <= ncol(cont/2)) == T) {
      new = rbind.data.frame(new,df[j,])  
    }
    j = j+1
}

# r trabaja a nivel de columna, si trabajamos a nivel de fila hay que especificar
# hay que trabajar a nivel de churro


      
}

rowSums(cont) >= ncol(cont)/2 # devuelve vector con tantos elementos como filas
  
# i = 1
# j = 1
# while (i <= ncol(numericas)) {
#   while (j <= nrow(numericas))
#   {
#     temp = FindOutliers(numericas[[i]]) # jugando con vector/columna
#     if (temp[j] == T)
#     {
#       numericas[j,] = NULL # analisis univariante, ya que igual a nivel de row 
#       # no es un outlier, aunq una columna si lo sea
#     }
#     j=j+1
#   }
#   i=i+1
# }

# i = 1
# j = 1
# while (i <= ncol(numericas)) {
#   while (j <= nrow(numericas))
#   {
#     temp = FindOutliers(numericas[[i]]) # jugando con vector/columna
#     if (temp[j] == T)
#     {
#       numericas[j,i] = NA #jugando con dataframe
#     }
#     j=j+1
#   }
#   i=i+1
# }

# library(mice)
# imputed_data = mice(numericas)


temp=mice(datosNum, m=6,maxit=10,meth="cart",seed=1234)
summary(temp)
datosNum1 <- complete(temp,1)
columnadatosNum = as.data.frame(apply(datosNum1,2,function(x) sum(is.na(x))))



# sacar outliers por cada variable

out1 = boxplot(numericas$age)
out2 = boxplot(numericas$hypertension)
out3 = boxplot(numericas$heart_disease)
out4 = boxplot(numericas$bmi)
out5 = boxplot(numericas$HbA1c_level)
out6 = boxplot(numericas$blood_glucose_level)

summary(numericas$age)

outliers_age = boxplot(numericas$age)$out
# para extrar la informacion de los outliers, "out" de la lista creada de la variable
# te lo guarda como vector



outliers_hypertension = boxplot(numericas$hypertension)$out
outliers_heart_disease = boxplot(numericas$heart_disease)$out
outliers_bmi = boxplot(numericas$bmi)$out
outliers_HbA1c_level = boxplot(numericas$HbA1c_level)$out
outliers_blood_glucose_level = boxplot(numericas$blood_glucose_level)$out

numericas_clean = cb


# outliers_G1 = boxplot(df3$G1)[["out"]]
# lo mismo que lo de arriba
# te lo guarda como vector

# Si los quisiéramos guardar como data frame
# out1_dataframe = as.data.frame(boxplot(df3$G1)$out) 


library(fastDummies)
dummies = fastDummies::dummy_columns(noNumericas, 
                                     remove_first_dummy = T,
                                     remove_selected_columns = T)

df2 = cbind(numericas, dummies)
sum(colSums(is.na(df2)))

# Busco outliers con DMwR2 y lofactor
library(DMwR2)
tmp = lofactor(numericas,k=3)
names(tmp) = "valores"

ggplot2::ggplot (tmp, aes(x = valores)) +
  geom_density() +
  geom_vline(xintercept = 2, color='red')



# Nos quedamos con aquellos registros por debajo del umbral
df2SinOutliers = df2 [tmp<=2,]

# Variables constantes, quito sólo aquellas con varianza = 0 y 
# cercanos a varianza 0
variables = nearZeroVar(df2SinOutliers, 
                        allowParallel = T, 
                        saveMetrics = T)

# columnas problemáticas
variables2 = nearZeroVar(df2SinOutliers, 
                         allowParallel = T)

# extracción sólo de las columnas constantes, con varianza 0 y colineales (nzv)
variables3 = row.names(variables[variables$zeroVar == F | 
                                   variables$nzv == F,])
df3 = df2SinOutliers[,variables3]
sum(is.na(df3))

# extracción de las columnas constantes, con varianza 0 y problemáticas (poca
# variabilidad). Ojo, porque la variable objetivo se elimina en este caso
df4 = df2SinOutliers[, c(variables2)]
df4$SalePrice = df2SinOutliers$SalePrice
sum(is.na(df4))


# Normalización de datos
# Puesto que he dummificado los datos categóricos, usaré normalización min max
# Nos quedamos con los valores máximos y mínimos de cada columna para luego deshacer
# la normalización

minimos_df3 = apply(df3, 2,min)
minimos_df4 = apply(df4, 2,min)
maximos_df3 = apply(df3, 2, max)
maximos_df4 = apply(df4, 2, max)

library(clusterSim)
df3N = data.Normalization(df3, 
                          type = "n4", 
                          normalization = "column")
df4N = data.Normalization(df4, 
                          type = "n4", 
                          normalization = "column")

sum(colSums(is.na(df3N)))
sum(colSums(is.na(df4N)))

# En el caso de la normalización en df4N tenemos Nas debido a que alguna
# columna es constante (!)
summary(df4$Functional_Sev)
summary(df4$RoofStyle_Shed)
df4N = df4N[,colSums(is.na(df4N))==0]

# Correlación entre variables predictoras.  
# Eliminamos aquellas cuya correlación sea >0.85
# Analizamos el VIF (variance inflaction factor) para cada variable predictora.
# El factor de inflación de la varianza cuantifica la intensidad de la 
# multicolinealidad en un análisis de regresión normal de mínimos cuadrados.

filtro3 = (names(df3N) %in% c("SalePrice"))==F
filtro4 = (names(df4N) %in% c("SalePrice"))==F


# Matriz de correlación
cor_matrix3N <- cor(df3N[,filtro3])
cor_matrix4N <- cor(df4N[,filtro4])




# Variables altamente correladas
alta_correlacion_3N <- caret::findCorrelation(cor_matrix3N, 
                                              cutoff = 0.85)
alta_correlacion_4N <- caret::findCorrelation(cor_matrix4N, 
                                              cutoff = 0.85)

# Eliminar correladas
df3NN <- df3N[, -alta_correlacion_3N]
df4NN <- df4N[, -alta_correlacion_4N]



######################################
# Modelos
set.seed(123)
acierto=c()
datos = df3NN
#datos = df4NN
y = which(names(datos) == "SalePrice")


indice = createMultiFolds(datos$SalePrice, k = 5, times = 1)
for (i in 1:length(indice)){
  datostrain = datos[ indice[[i]],]
  datostst = datos[-indice[[i]],]
  regresion = lm(SalePrice~., data=datostrain)
  prediccion=predict(regresion,datostst)
  resultado=1-(sum((datostst[,y]-prediccion)^2)/sum((datostst[,y]-mean(datostst[,y]))^2))
  acierto = rbind(acierto,c(resultado))
}
mean(acierto)
regresion = lm(SalePrice~., data=datos)
summary(regresion)
acierto

aciertoarbol=c()
#indice = createMultiFolds(datos$SalePrice, k = 5, times = 1)
for (i in 1:length(indice)){
  datostrain = datos[ indice[[i]],]
  datostst = datos[-indice[[i]],]
  arbol = rpart::rpart(SalePrice ~ ., data = datostrain, maxdepth = 4)
  prediccion = predict(arbol,datostst)
  resultado = 1-(sum((datostst[,y]-prediccion)^2)/
                   sum((datostst[,y]-mean(datostst[,y]))^2))
  aciertoarbol = rbind(aciertoarbol,c(resultado))
}

# Graficamos 
rpart.plot::prp(arbol, cex=.4,main="Arbol")
rpart.plot::rpart.plot(arbol,cex=0.75)

# Acierto
mean(aciertoarbol)

# Nodos terminales
length(unique(prediccion))
length(unique(datostst$strength))

# Error  
ver = cbind.data.frame(datostst$SalePrice,prediccion)
ver$Error = ver$`datostst$SalePrice`-ver$prediccion
mean(abs(ver$Error))
quantile(ver$Error,probs = c(0.05,0.95))
hist(ver$Error,main = 'Histograma del error', xlab = 'Error' )
plot(ver$`datostst$SalePrice`,ver$prediccion)


aciertoRF=c()
aciertoRFtrain=c()
#indice = createMultiFolds(datos$strength, k = 5, times = 1)
for (i in 1:length(indice)){
  datostrain = datos[ indice[[i]],]
  datostst = datos[-indice[[i]],]
  modeloRF = randomForest::randomForest(SalePrice ~ ., data=datostrain, ntree=1000, mtry=3)
  predicciontrain=predict(modeloRF,datostrain)
  prediccion=predict(modeloRF,datostst)
  resultado=1-(sum((datostst[,y]-prediccion)^2)/sum((datostst[,y]-mean(datostst[,y]))^2))
  resultadotrain=1-(sum((datostrain[,y]-predicciontrain)^2)/sum((datostrain[,y]-mean(datostrain[,y]))^2))
  aciertoRF = rbind(aciertoRF,c(resultado))
  aciertoRFtrain = rbind(aciertoRFtrain,c(resultadotrain))
}

ver = cbind.data.frame(datostst$SalesPrice,prediccion)
ver$Error = ver$`datostst$SalesPrice`-ver$prediccion
mean(abs(ver$Error))
quantile(ver$Error,probs = c(0.05,0.95))
aciertoRF
mean(aciertoRF)
mean(aciertoRFtrain)
aciertototal=cbind(aciertoRF,aciertoRFtrain)
aciertototal
importanciarf=as.data.frame(importance(modeloRF))library(DMwR2)
tmp = data.frame(lofactor(df2,k=5))
names(tmp) = "valores"
view(tmp)

ggplot2::ggplot (tmp, aes(x = valores)) +
  geom_density() +
  geom_vline(xintercept = 2, color='red')

# Nos quedamos con aquellos registros por debajo del umbral
df2SinOutliers = df2 [tmp<=2,]

# Variables constantes, quito sólo aquellas con varianza = 0 y 
# cercanos a varianza 0
variables = nearZeroVar(df2SinOutliers, 
                        allowParallel = T, 
                        saveMetrics = T)

# columnas problemáticas
variables2 = nearZeroVar(df2SinOutliers, 
                         allowParallel = T)



# extracción sólo de las columnas constantes, con varianza 0 y colineales (nzv)
variables3 = row.names(variables[variables$zeroVar == F | 
                                   variables$nzv == F,])
df3 = df2SinOutliers[,variables3]
sum(is.na(df3))

# extracción de las columnas constantes, con varianza 0 y problemáticas (poca
# variabilidad). Ojo, porque la variable objetivo se elimina en este caso
df4 = df2SinOutliers[, c(variables2)]
df4$SalePrice = df2SinOutliers$SalePrice
sum(is.na(df4))


# Normalización de datos
# Puesto que he dummificado los datos categóricos, usaré normalización min max
# Nos quedamos con los valores máximos y mínimos de cada columna para luego deshacer
# la normalización

minimos_df3 = apply(df3, 2,min)
minimos_df4 = apply(df4, 2,min)
maximos_df3 = apply(df3, 2, max)
maximos_df4 = apply(df4, 2, max)

library(clusterSim)
df3N = data.Normalization(df3, 
                          type = "n4", 
                          normalization = "column")
df4N = data.Normalization(df4, 
                          type = "n4", 
                          normalization = "column")

sum(colSums(is.na(df3N)))
sum(colSums(is.na(df4N)))

# En el caso de la normalización en df4N tenemos Nas debido a que alguna
# columna es constante (!)
summary(df4$Functional_Sev)
summary(df4$RoofStyle_Shed)
df4N = df4N[,colSums(is.na(df4N))==0]

# Correlación entre variables predictoras.  
# Eliminamos aquellas cuya correlación sea >0.85
# Analizamos el VIF (variance inflaction factor) para cada variable predictora.
# El factor de inflación de la varianza cuantifica la intensidad de la 
# multicolinealidad en un análisis de regresión normal de mínimos cuadrados.

filtro3 = (names(df3N) %in% c("SalePrice"))==F
filtro4 = (names(df4N) %in% c("SalePrice"))==F


# Matriz de correlación
cor_matrix3N <- cor(df3N[,filtro3])
cor_matrix4N <- cor(df4N[,filtro4])




# Variables altamente correladas
alta_correlacion_3N <- caret::findCorrelation(cor_matrix3N, 
                                              cutoff = 0.85)
alta_correlacion_4N <- caret::findCorrelation(cor_matrix4N, 
                                              cutoff = 0.85)

# Eliminar correladas
df3NN <- df3N[, -alta_correlacion_3N]
df4NN <- df4N[, -alta_correlacion_4N]



######################################
# Modelos
set.seed(123)
acierto=c()
datos = df3NN
#datos = df4NN
y = which(names(datos) == "SalePrice")


indice = createMultiFolds(datos$SalePrice, k = 5, times = 1)
for (i in 1:length(indice)){
  datostrain = datos[ indice[[i]],]
  datostst = datos[-indice[[i]],]
  regresion = lm(SalePrice~., data=datostrain)
  prediccion=predict(regresion,datostst)
  resultado=1-(sum((datostst[,y]-prediccion)^2)/sum((datostst[,y]-mean(datostst[,y]))^2))
  acierto = rbind(acierto,c(resultado))
}
mean(acierto)
regresion = lm(SalePrice~., data=datos)
summary(regresion)
acierto

aciertoarbol=c()
#indice = createMultiFolds(datos$SalePrice, k = 5, times = 1)
for (i in 1:length(indice)){
  datostrain = datos[ indice[[i]],]
  datostst = datos[-indice[[i]],]
  arbol = rpart::rpart(SalePrice ~ ., data = datostrain, maxdepth = 4)
  prediccion = predict(arbol,datostst)
  resultado = 1-(sum((datostst[,y]-prediccion)^2)/
                   sum((datostst[,y]-mean(datostst[,y]))^2))
  aciertoarbol = rbind(aciertoarbol,c(resultado))
}

# Graficamos 
rpart.plot::prp(arbol, cex=.4,main="Arbol")
rpart.plot::rpart.plot(arbol,cex=0.75)

# Acierto
mean(aciertoarbol)

# Nodos terminales
length(unique(prediccion))
length(unique(datostst$strength))

# Error  
ver = cbind.data.frame(datostst$SalePrice,prediccion)
ver$Error = ver$`datostst$SalePrice`-ver$prediccion
mean(abs(ver$Error))
quantile(ver$Error,probs = c(0.05,0.95))
hist(ver$Error,main = 'Histograma del error', xlab = 'Error' )
plot(ver$`datostst$SalePrice`,ver$prediccion)


aciertoRF=c()
aciertoRFtrain=c()
#indice = createMultiFolds(datos$strength, k = 5, times = 1)
for (i in 1:length(indice)){
  datostrain = datos[ indice[[i]],]
  datostst = datos[-indice[[i]],]
  modeloRF = randomForest::randomForest(SalePrice ~ ., data=datostrain, ntree=1000, mtry=3)
  predicciontrain=predict(modeloRF,datostrain)
  prediccion=predict(modeloRF,datostst)
  resultado=1-(sum((datostst[,y]-prediccion)^2)/sum((datostst[,y]-mean(datostst[,y]))^2))
  resultadotrain=1-(sum((datostrain[,y]-predicciontrain)^2)/sum((datostrain[,y]-mean(datostrain[,y]))^2))
  aciertoRF = rbind(aciertoRF,c(resultado))
  aciertoRFtrain = rbind(aciertoRFtrain,c(resultadotrain))
}

ver = cbind.data.frame(datostst$SalesPrice,prediccion)
ver$Error = ver$`datostst$SalesPrice`-ver$prediccion
mean(abs(ver$Error))
quantile(ver$Error,probs = c(0.05,0.95))
aciertoRF
mean(aciertoRF)
mean(aciertoRFtrain)
aciertototal=cbind(aciertoRF,aciertoRFtrain)
aciertototal
importanciarf=as.data.frame(importance(modeloRF))