# Monitoria 03/04/2021 - 10/04/2021------

rm(list = ls())
# para identificar valores vacios
X <- NA
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

library(readxl)

data <- read_excel("Tabla.xlsx")
View(data)

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

m <- matrix(5:8, nrow = 2, ncol = 2)
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
#1373 MB --> 1.37 GB --> si su computador tiene menos... llorinder :´(
#y de hecho se necesita un poco más porque es un cálculo estimado si todos son numericos
#la heuristica es que se necesita el doble de lo calculado (2.7 GB)



#curso de R desde lo más básico en Coursera (gratís) -------
# https://www.coursera.org/learn/r-programming

# El poder del inteto y el error: ------
# https://www.youtube.com/watch?v=K5wCfYujRdE -------

#10-04-21 -------

#dump y dput preservan metadata (no tener que especificarlo cada vez)
# facil corregir errores en el archivo si lo hay
#pero no es muy eficiente en términos de espacio

y <- data.frame(a=1, b= "a")
dput(y)

dput(y, file = "y.R")
new.y <- dget("y.R") # permite reconstruir un objeto de R
new.y

#pueden guardarse multiples objetos y leerlos con la función source

x <- "foo"
y <- data.frame(a=1, b="a")
dump(c("x", "y"), file = "data.R") #observen que en este caso se colocaron entre comillas

x <- 0
y <- 0

source("data.R")
y
x

#connections -----

#file <- a un archivo
#gzfile <- a un archivo comprimido con gzip
#bzfile <- a un archivo comprimido con bzip2
#url <- a una página web

#usualmente no muy necesario: 
#   con <- file("foo.txt", "r")
#   data <- read.csv(con)
#   close(con)

#esas tres lineas se resumen en data <- read.csv("foo.txt")

#esto puede tomar algún tiempo: 

con <- url("http://www.jhsph.edu", "r")
x <- readLines(con)
head(x)

con <- url("https://es.wikipedia.org/wiki/Casa_Blanca", "r")
wh <- readLines(con)
head(wh)

#Subsetting -------

#[] devuelve un elemento igual al que se le esta sacando el subconjunto
#[[]] para obtener elemento de una lista o Dataframe (solo 1)
# $ para extraer elementos por su nombre.

x <- c("a", "b", "c")
x[1]
x[3]
x[1:3] #usando index numérico

x[x > "a"] #index lógico (las letras también tienen "un orden")

(u <- x > "a") #vector lógico
x[u]

#para una lista: 

x <- list(index = 1:4, puntaje = 0.6, nombre = c("Juan", "Pepito", "Pepita", "Juana"))
x

x[1] #esta tambien es una lista!

x[[1]] #por otro lado, esto es un vector.

x$nombre
x$puntaje
x[["puntaje"]]
x["puntaje"] #lista

x[c(1,3)] #extraer varios elementos de la lista

x[c(1,"nombre")]  #toma ambos como caracteres 
x[c("index", "nombre")]

#dobble corchete para acceder a elementos de los elementos
x[[c(3, 1)]]
x[[3]][[1]]

#para una matriz podemos dejar en blanco la columna o fila para indicar que se
# desea completa

X <- matrix(1:6, 2, 3)
X
X[1,]
X[,3] #pero en el caso de matrices por default devuelve vectores y no matrices
class(X[,3])
#lo anterior se su puede cambiar con drop=F

X[1, 2, drop=F]
class(X[1, 2, drop=F])

X[1:2, 1:2]

#partial matching -------

x <- list(aljfdsmn=1:5, basdnfoa=10:15)
x$a
x[["a"]]
x[["b", exact=F]]

#Removing NA values ------
x <- c(1, 2, NA, 5)
bad <- is.na(x)
bad
x[!bad] #con el operador ! se retiran los elementos que deseamos eliminar

#la función complete.cases identifica que los vectores esten completos

x <- c(1,2, NA, 3, NA)
y <- c(NA, 5, 1, 4, NA)

good <- complete.cases(x, y) #funciona de la misma manera para dataframes.
good
x[good]
y[good]

#vectorized operations -------
#hacer el código más eficiente

x <- 1:4; y <- 6:9
x+y

x>2
x>=2
y == 8
x*y #mutiplica uno por uno
x/y

x<- matrix(1:4, 2, 2); y <- matrix(rep(10, 4),2,2)
x*y #elemento por elemento
x/y
x %*% y #multiplicación adecuada de matrices

#ejercicio de Bono - 7 min------

data <- read.csv("hw1_data.csv")

#1. nombres de las columnas?
#2. número de observaciones?
#3. valor ozono en fila 47?
#4. cuantas filas completas?
#5. máximo ozono en mes de mayo?
#1,2,3
View(data)
data[1:2,]
data[152:153,]
data[47,]
#4.
completas <- complete.cases(data)
completas
sum(completas)

mean(data$Ozone, na.rm = T)
# Extract the subset of rows of the data frame where Ozone 
#values are above 31 and Temp values are above 90. 
#What is the mean of Solar.R in this subset?

complete <- data[completas,]
complete1 <- complete[complete$Ozone >31 & complete$Temp>90,]
mean(complete1$Solar.R)

month6 <- data[data$Month == 6,]
mean(month6$Temp)
#5
may <- complete[complete$Month == 5,]
may
max(may$Ozone)

#regresión multiple -------


library(readxl)


tabla <- read_excel("Tabla1.xlsx")
View(tabla)

(x1<- tabla$x1_t)
x2<- tabla$x2_t
x3<- tabla$x3_t
y<- tabla$y_t

summary(tabla)
modelo<-lm(y~x1+x2+x3-1)

library(stargazer)
install.packages("svyglm")
library(car)
stargazer(modelo, type="text") #ojo, forzando a pasar por el origen
?`LinearMethodsList-class`

#prueba de hipotesis
linearHypothesis(modelo, c("x1 = 0"))
linear.hypothesis(modelo, c("x1 = 1"))



modelo1 <- lm(y~x1+x2+x3)
stargazer(modelo1, type="text")

vcov(modelo)
yest <- 1.6*x1+0.8730*x2+0.9683*x3
yest
errores_est<- y-yest
errores_est
mean(errores_est)
sum(errores_est)

sqrt(vcov(modelo))

