#Eliminamos todo lo anterior

rm(list=ls())

#Cargar un CSV a partir de una determinada fila e indicarle la separacion.

datos = read.csv("consumoelectricov1.csv") #da error
datos = read.csv("consumoelectricov1.csv",skip = 33,sep=";")

# Al observar los datos vemos que no estan en formato numerico
# En R el separador de decimal es el punto y aqui tenemos ","
# Tambien tenemos un separador de miles que es un punto 

# Corregimos ese problema para 2015.

datos$X2015 = as.character(datos$X2015)     # Convierto en texto
datos$X2015 = gsub("\\.", "", datos$X2015)  # Borro el punto (o lo reemplazo por nada)
datos$X2015 = gsub("\\,", ".", datos$X2015) # Cambio la coma por punto
datos$X2015 = as.numeric(datos$X2015)       # Lo convierto en numerico


#Puedo hacerlo todo a la vez o ir 1 a 1
#Escribo la funcion en orden contrario a lo que quiero

datos[,4:16] = apply(datos[,4:16],2,function(x) as.numeric(gsub("\\,",".",gsub("\\.","",as.character(x)))))

#Esto tambien se puede hacer con el excel pero hasta un limite de datos.

# ?Puedo mejorar los datos con la info que tengo?
# ?El codigo de municipio me sirve para algo?


#Puedo saber a traves del codigo a que provincia pertenece?

#Creo la variable provincia

datos$provincia = "XXX"
table(datos$provincia)
datos$provincia[datos$Codigo.municipio<15000] = "Alava"
datos$provincia[datos$Codigo.municipio>15000 & datos$Codigo.municipio<40000] = "Gipuzkoa"
datos$provincia[datos$Codigo.municipio>40000] = "Bizkaia"
table(datos$provincia)

#De cara a establecer comparaciones podemos calcular el gasto medio por municipio

datos$promedio = rowMeans(datos[,3:16])
datos$promedio2 = colMeans(datos[,3:16])

#Tambien podemos determinar en funcion del consumo si es un pueblo o una ciudad.
# En este caso podemos poner el limite en 50.000

datos$pueblociudad = "XXX"
datos$pueblociudad[datos$X2015>50000] = "Ciudad"
datos$pueblociudad[datos$X2015<=50000] = "Pueblo"
table(datos$pueblociudad)


#Tambien podemos seleccionar los municipios con menor/mayor consumo
datos[c(6,9,85,7),]
datosmaximos   = datos[order(datos$X2015,decreasing=TRUE)[1:20],]
datosminimos   = datos[order(datos$X2015,decreasing=FALSE)[1:20],]


#O los de una determinada provincia

datosBizkaia   = datos[datos$provincia=="Bizkaia",]
datosAlava     = datos[datos$provincia=="Alava",]
datosGipuzkoa = datos[datos$provincia=="Gipuzkoa",]


# Otra opcion es saber cuanto es el consumo maximo/minimo

menorconsumo = min(datos$promedio)
mayorconsumo = max(datos$promedio)


# ?No os falta algo en este ultimo dato?
datos$Municipio[datos$promedio == max(datos$promedio)]

#Tambien podemos pedir el nombre del municipio con mayor/menor consumo

municipio_menorconsumo = as.data.frame(datos$Municipio[which.min(datos$promedio)])
municipio_mayorconsumo = as.data.frame(datos$Municipio[which.max(datos$promedio)])

# O el nombre de la provincia donde esta el municipio con mayor/menor consumo

provincia_menorconsumo = datos$provincia[which.min(datos$promedio)]
provincia_mayorconsumo = datos$provincia[which.max(datos$promedio)]

# Otra opcion es clasificar los municipios en consumo alto/medio/bajo segun sus percentiles

consumo_p33_provincia = aggregate(promedio~provincia,data=datos,FUN = quantile, probs  = 0.33)
consumo_p66_provincia = aggregate(promedio~provincia,data=datos,FUN = quantile, probs  = 0.67)

tabla = merge(consumo_p33_provincia,consumo_p66_provincia,by="provincia")

# En este caso nos interesa cambiar los nombres de las variables para saber que son

colnames(tabla)[2:3]=c("p33","p66")

#Metemos la tabla en los datos

datos = merge(datos,tabla,by="provincia")

datos$consumoProvincia = "XXX"
datos$consumoProvincia[datos$promedio<datos$p33] = "Bajo"
datos$consumoProvincia[datos$promedio>=datos$p33 & datos$promedio<datos$p66] = "Medio"
datos$consumoProvincia[datos$promedio>=datos$p66] = "Alto"
table(datos$consumoProvincia)


# Tambien podemos calcular las diferencias en el consumo a lo largo del periodo.

datos$dif1502 = datos$X2015-datos$X2002

# Y ordenarlo segun esas diferencias

datos = datos[order(datos$dif1502),]

# Y mostrar los 10 en los que mas/menos se ha incrementado el consumo.
# Podemos mostrar las columnas que nos interese

head(datos[,c(1,3,23)],n=10)
tail(datos[,c(1,3,23)],n=10)

# O todas

head(datos)
