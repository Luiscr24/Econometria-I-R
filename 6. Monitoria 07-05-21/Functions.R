#crear una función sencilla ------

add_2 <- function(x,y) {
  x+y
}

add_2(3,4)

above_10 <- function(x){
  use <- x  >10 #crea un vector lógico
  x[use]
}

above <- function(x,n){
  use <- x >n
  x[use]
}

above_10(c(1,2,4,20,19,12))
above(c(1,2,4,20,19,12), 12)

#default arguments

above_1 <- function(x, n=10){
  use <- x >n
  x[use]
}

above_1(c(1,2,4,20,19,12))

column_mean <- function(x){
  nc <- ncol(x)
  means <- numeric(nc) #creamos un vector vacio con nc elementos
  for(i in 1:nc) means[i] <- mean(x[,i], na.rm = TRUE)
  means
}

column_mean(airquality)

#functions son objetos de primera clase (como cualquier otro objeto)
#they can be nested. 
#tienen name arguments (pueden tener valor default)
#argumentos pueden tener match por nombre o por orden en que se colocan.

# el argumento ... suele usarse cuando se estan replicando argumentos
#de otra función para no tener que escribirlos de nuevo

myplot <- function(x, y, type="l", ...){
  plot(x, y, type=type, ...)
}

#.... tambien cuando no se conoce cantidad de argumentos integrados a la 
#función no se pueden conocer con aterioridad.

args(paste)
args(cat)

search() #orden en que r busca por los objetos que llamamos, es decir
#que el orden en que llamamos los paquetes importa. 

#qué es un environment? -----
#una colección de pares (symbol, value)

make.power <- function(n){
  pow <- function(x){
    x^n
  }
  pow
}

cube <- make.power(3)
square <- make.power(2)

cube(4)
square(4)
