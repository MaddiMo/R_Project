#1- Realizar las siguientes operaciones:

#1.1
a = 25.3
b = 35.1
x = a + b
x

#1.2
a = 15
b = 8.2
x = a - b
x

#1.3
a = 12
b = 4
c = a * b
c

#1.4
a = 15
b = 3
d = a / b
d

#1.5
e = sqrt(125)
e

#1.6
a = 2
b = 8
f = a^b
f


#3 Crear los siguientes vectores:

#3.1
a = 20:127
a

#3.2
b = c(5, 12, 25, 8, 46, 52)
b

#3.3
c = c(35:80, 10, 100)
c 

#3.4
d = c("a", "b", "c", "d", "e", 1, 2, 3, 4, 5)
d

#4. Realizar las siguientes tareas con vectores:

#4.1.
length(a)
length(b)
length(c)

#4.2
vector_1 = a[1:5]
vector_1

#4.3.
vector_2 = a[a > 119] #el elemento comienza en 20, por lo que el elemento numero 100 es el numero 120
vector_2

vector_3 = a[-c(1:100)]
vector_3

#4.4
vector_4 = a[a <= 99]
vector_4

#4.5
class(a)

#4.6.
vector_6 = as.character(a)
vector_6

ls()

#5 Crear las siguientes listas y realizar las siguiente tareas

#5.1.

mylist = list(name = c("Fred", "Juan", "Pepito", "Lola", "Maria"), edad = c(33, 45, 14, 17, 67), altura = c(165, 178, 176, 189, 156))
mylist

#5.2

mylist$name
mylist["name"]

mylist$edad
mylist$altura

#5.3

mylist[2]

#5.4

mylist[[2]][1]

#5.5

mylist[[1]][5]

#6. Crear una matriz de 4x4 y realizar las siguientes tareas:

#6.1.

new_matrix = matrix(c(1,2,3,4, 5,6,7,8, 10,11,12,13, 14,15,16,17),
              nrow = 4, ncol = 4, byrow = TRUE,
              dimnames = list(c("row1", "row2", "row3", "row4"), c("V1", "V2", "V3", "V4")))
new_matrix

#6.2
new_matrix
attributes(new_matrix)

#6.3
new_matrix[1:2,]

#6.4
new_matrix[,1:2]

#6.5
new_matrix[1:2,1:3]

#6.6.
new_matrix
new_matrix = cbind(new_matrix, V5= c(1,2,3,0))
new_matrix

#6.7
new_matrix = rbind(new_matrix, row5= c(9,23,4,5,4))
new_matrix

#7.Crear un factor con las notas que se pueden sacar en la Universidad 
# (SS, AP, NT, SB y MH) y realizar las siguientes tareas:
notas = factor(c("SS","AP","NT","SB","MH"))
notas

#7.1. Calcular el número de opciones existentes.
nlevels(notas)

#7.2. Mostrar los distintos niveles
levels(notas)

#7.3. Mostrar el tercer nivel.
levels(notas)[3]

#7.4 Sustituir “NT” por “Notable”.

levels(notas)[3] = "Notable"
levels(notas)

#8- Crear un DataFrame de 4 variables (Nombre del Jugador, Equipo, Posición y Edad) y 8
# observaciones (Jugadores) y realizar las siguientes tareas:

mi_tabla = data.frame(Jugadores=c("Maria","Rita","Paquita","Juan","Pepa","Jon","Unai","Pepe"),
                      Equipo=c("Rojo","Verde","Amarillo","Azul","Violeta","Naranja","Blanco","Negro"),
                      Posición=c(1,2,3,4,5,6,7,8), Edad=c(23,43,23,45,65,45,76,77))
#8.2
mi_tabla

#8.3 - 8.4 - 8.5
ncol(mi_tabla)
nrow(mi_tabla)
dim(mi_tabla)

#8.6

colnames(mi_tabla) = c("Nombre","Team","Especialidad","Años")
mi_tabla

#8.7 - 8.8. - 8.9 - 9.10
mi_tabla[mi_tabla$Años>25,]
mi_tabla[mi_tabla$Años<25 & mi_tabla$Años>20,]
mi_tabla[mi_tabla$Años<25 & mi_tabla$Especialidad == 3,]
mi_tabla[mi_tabla$Años<25 | mi_tabla$Especialidad == 5,]

#9. Crear las siguientes funciones:
#9.1. Determine el área de un círculo.

area_circulo = function(r) {
  x = pi*r^2
  return(x)
}
area_circulo(6)

area_circulo_2=function(r) {pi*r^2}
area_circulo_2(5)

#9.2.Determine el área de un rectángulo.

area_rectangulo = function(x, y) {
  z = x*y
  return(z)
}
area_rectangulo(6,8)

area_rectangulo_2 = function(x,y){x*y}
area_rectangulo_2(3,5)

#9.3.Resuelva una ecuación de segundo grado.

ecuacion = function(a,b,c){
  x =(-b+-sqrt(b^2-4*a*c))/(2*a)
  return(x)
}
ecuacion(2,40,5)

#9.4.Determine el perímetro de una circunferencia.

#10. Crear una condicion que determine si una letra es vocal
#o consonante o si se trata de la Y griega.

x="f"
if (x=="a"|x=="e"|x=="i"|x=="o"|x=="u"){
  print("vocal")
} else {
  print("consonante")
}
