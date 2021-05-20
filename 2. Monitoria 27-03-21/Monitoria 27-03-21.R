# Monitoria 27/03/2021 ------

# Problema con la grabación previa -> se actualizó o pueden verla directamente en Teams.
rm(list = ls())

# <- es el operador de asignación

msg <- "Hola a todos y todas"
msg
print(msg)

# [1] indica que msg es un vector y que el string es el primer elemento.

(y <- 1:15) #crear secuencia de números

(z <- 1:100) #en este caso [i] nos muestra la posición correspondiente al primer
            #elemento de la fila

# atomic classes ------
#en R hay 5: character, numeric (real numbers), integer, complex, logical (FALSE, TRUE)

# vectores y listas ------
# un vector es un objeto cuyos elementos son de la misma clase
# Una lista puede contener elementos de diferentes clases.

?vector() #para crear un vector vacio

X <- vector(mode = "character", length = 10) #para especificar longitud y clase
X

1L #L para indicar explicitamente que es un entero y no un número con decimales

Inf #es un entero que se puede usar para simular infinidad

(cero <- 1/Inf)

NaN # no es un número: 

(no_number <- 0/0)

#atributos -------
#los objetos tienen atributos
  #names, dimnames
  #dimensions (e.g. matrices, arrays)
  #class
  #length
  #otros definidos por el usuario

#crear vectores ------

x <- c(0.5, 0.6) #numeric
y <- c(TRUE, FALSE) #logical --> Ojo con las mayusculas
z <- c(T, F) #logical
w <- c("a", "b", "c")
(v <- 9:29) #integer
(l <- c(1+0i, 2+4i)) #complex

X <- vector("numeric", length = 5)
X

#coercion para que los elementos sean del mismo tipo 

(q <- c(1.7, "a")) ##character
(e <- c(TRUE, 2)) ##numeric (por convención TRUE=1, FALSE=0)
(r <- c(TRUE, "econometría")) #char

# explicit coercion -----
#permite convertir objetos al tipo que deseamos

x <- 0:6
class(x)

as.numeric(x)

as.logical(x)

as.character(x)

#fechas, puede ser util usar formate as.Date, especialmente para series de tiempo
hoy <- "27/03/2021"
class(hoy)

as.Date(hoy, format ="%d/%m/%Y")

hoy1 <- "27-03-2021"
class(hoy1)

as.Date(hoy1, format ="%d-%m-%Y")
class(as.Date(hoy1, format ="%d-%m-%Y"))

#la coerción no siempre funciona

x <- c("a", "b", "c")
as.numeric(x)
as.logical(x)
as.complex(x)

#Listas -> diferentes clases -------

#elementos de una lista tienen doble corchete cuadrado
t <- list(1, "a", "b", TRUE, 1+4i)
t #presenta cada elemento como un vector

#matriz -----

m<- matrix(nrow = 2, ncol = 3)
dim(m)
attributes(m)
m
m <- matrix(1:6, nrow = 2, ncol = 3)
m

#otra posible forma es: 

(m <- 1:10)
dim(m) <- c(2,5)
m

#o biding columnas y filas

x<- 1:3 
y<- 10:12

cbind(x, y) #forma rápida de unir los datos de las variables que tenemos por  columnas

#pero también se puede por filas

rbind(x, y)

# Factores, para datos categóricos -------

#ejemplos: TPM; confianza en el gobierno, calificación (malo, neutral, bueno, sobresaliente)
#se refiere a etiquetas --> pueden estar ordenados o no ordenados 
# una variable categorica dice más que "1, 2, 3, ...".

f <- factor(c("yes", "no", "yes", "no", "maybe"))
f
table(f)
unclass(f) #permite asignar números a cada clase (3=yes, 2=no y maybe=1)
attr(f, "levels")
attributes(f)

#podemos establecer el orden de los factores también es posible
f <- factor(c("yes", "no", "yes", "no", "maybe"), levels = c("yes","no", "maybe"))
f
unclass(f)


#como realizar una regresión de OLS sencilla -----
library(readxl)

data <- read_excel("Tabla.xlsx")
View(data)

data2 <- read_excel("Tabla.xlsx", skip = 2) #cuantos elemenos saltar antes de leer.
View(data2)

data3 <- read_excel("Tabla.xlsx", n_max =  8) #leer solo las primeras 8 filas
View(data3)

(y <- data$y_t)
(x <- data$x_t)

str(data)
modelo <- lm(y~x)
summary(modelo)

library(stargazer)
stargazer(modelo, type="text")
vcov(modelo)

plot(x ~ y, main="relación", type="p",
     
     sub= "subtitulo",
     xlab="variable x",
     ylab="variable y",
     #asp=2 #ratio y/x
) #lineas y puntos

plot(x ~ y, main="relación", type="l",
     #main="",
     sub= "subtitulo",
     xlab="variable x",
     ylab="variable y",
     #asp=2 #ratio y/x
) #lineas y puntos

plot(x ~ y, main="relación", type="b",
     #main="grafico raro",
     sub= "gráfico",
     xlab="variable x",
     ylab="variable y",
     #asp=2 #ratio y/x
) #lineas y puntos

plot(x ~ y, main="relación", type="o") #overplotted
plot(x ~ y, main="relación", type="h") #histogram
plot(x ~ y, main="relación", type="s") #

library(ggplot2)
grafica_1 = ggplot(modelo, aes(x=x, y=y)) + geom_point(col="brown") +geom_smooth(method = "lm")
grafica_1

grafica_2 <- grafica_1 +labs(title="gráfico sencillo", subtitle = "", 
                             y= "y", x= "x", caption = "Monitoria Econometría")
grafica_2
?plot


# para identificar valores vacios
is.na(X) # --> operaciones matemáticas no identificadas
is.nan(X) # --> todo lo demás.

# NA tiene diferentes clases (char, numeric, etc) --> NAN es NA pero no visceversa

X <- c(1,2, NA)
is.na(X)
is.nan(X)

X <- c(1,2, NaN)
is.na(X)
is.nan(X)

# Data Frames ------
#usados para información de forma tabular
#a diferencia de las matrices pueden tener objetos de diferentes clases (como las listas)
#se crean con read.table() o con read.csv()

row.names(data) #"nombre de cada observación" --> util para interpretar datos y organizar
colnames(data)
data.matrix(data) #--> para convertirla en matrix si es postible


D <- data.frame(y=1:10, estudiante = c(F,F,F,F,T,F,T,T,T,T))
D
nrow(D)
ncol(D)


# Names ------
#Podemos asociar nombres a las variables dentro de cada objeto

x <- c(1,2,3)
names(x) <- c("hola", ":)", "amiwitos")
x
names(x)

#para listas
x <- list(a=1, b="2021", c=1+4i)
x

#para matriz

m <- matrix(5:9, nrow = 2, ncol = 2)
dimnames(m) <- list(c("niñxs", "jovenes"), c("helado", "torta"))
m

#Reading data -------

#read.table
#read.csv
#readLines --> leer lineas de un archivo de texto
#source/dget --> leer codigo en R (de otros archivos)
#load --> leer desde espacios de trabajo guardados 
#userialize --> leer objetos de R en codigo binario.

#para cantidad de datos pequeños se puede usar table sin especificar nada más

?sapply #permite aplicar una función sobte un vector o una lista

classes <- sapply(data, class)
classes #forma sencilla de conocer las clases de las columnas en el data frame


#Calcular requerimientos de memoria RAM -------

#aproximación --> cada dato numperico --> 8 bytes

#si tenemos 1500000 filas y 120 columnas

1500000*120*8 #bytes

#eso debemos dividirlo por 2^20 (bytes/MB)

1500000*120*8/(2^20)
#1373 MB --> 1.34 GB --> si su computador tiene menos... llorinder :´(
#y de hecho se necesita un poco más porque es un cálculo estimado si todos son numericos
#la heuristica es que se necesita el doble de lo calculado (2.7 GB)



#curso de R desde lo más básico en Coursera (gratís) -------
# https://www.coursera.org/learn/r-programming

# El poder del inteto y el error: ------
# https://www.youtube.com/watch?v=K5wCfYujRdE -------
