

# Ojo evite usar tildes o "ñ" ('escriba en ingles')

#Limpiar entorno de trabajo -----

rm(list=ls())


#install.packages("tidyverse")
library(tidyverse) #data manipulation and plotting functions
#install.packages('tsibble')
library(tsibble) #timeseries manupulation
#install.packages('tsibbledata')
library(tsibbledata) #tidy time series
#install.packages('feasts')
library(feasts) #graphics time series
#install.packages('fable')
library(fable) # forcasting

# ------ 0. Podemos simplificar instalando --------

install.packages(c("tidyverse","fpp3"))
library(fpp3)
library(tidyverse)

# Plot one time series: Hay datos ya guardados en los repositorios de r ------



aus_retail %>%
  filter(`Series ID`=="A3349640L") %>% #este simbolo para aplicar funciones
  autoplot(Turnover)

# Produce some forecasts
aus_retail %>%
  filter(`Series ID`=="A3349640L") %>%
  model(ETS(Turnover)) %>%
  forecast(h = "2 years")


#tiverse contiene: 
# ggplot2
# dplyr
# tidyr
# readr
# purrr
# tibble
# stringr
# forcats

library(here)
?here #forma sencilla de conocer para que sirven


# janitor: Simple Tools for Examining and Cleaning Dirty Data
library(janitor)

#skimr: Compact and Flexible Summaries of Data
library(skimr)
library(readxl)

roster_raw <- read_excel("C:/Users/ADMIN/OneDrive - Universidad Nacional de Colombia/Escritorio/OneDrive - Universidad Nacional de Colombia/Luis/Econometria I/Mi Monitoria/1. Intro/1. dirty_data.xlsx")
#ojo es / no \ como viene en propiedades

roster_raw <- read_excel(here("1. dirty_data.xlsx"))

glimpse(roster_raw)
view(roster_raw)

#clean_names() is a convenience version of make_clean_names() that can be used for piped 
#data.frame workflows.

roster_raw <- roster_raw %>%
  row_to_names(row_number = 1) %>%
  clean_names()
view(roster_raw)

roster <- roster_raw %>%
  remove_empty(c("rows", "cols")) %>%
  remove_constant(na.rm = TRUE, quiet = FALSE) 

view(roster)

roster$employee_status

roster <- roster_raw %>%
  remove_empty(c("rows", "cols")) %>%
  remove_constant(na.rm = TRUE, quiet = FALSE) %>% 
  mutate(hire_date = convert_to_date(hire_date, #handle the mixed-format dates
                                 character_fun = lubridate::mdy),
  cert = dplyr::coalesce(certification, certification_2))%>%
  select(-certification, -certification_2) # drop unwanted columns
  #> Removing 1 constant columns of 10 columns total (Removed: active).

view(roster)

#Use get_dupes() to identify and examine duplicate records during data cleaning. 
#Let’s see if any teachers are listed more than once:

roster %>% get_dupes(contains("name"))

get_dupes(roster, contains("name"))
?get_dupes
#A variable (or combinations of two or three variables) can be tabulated with tabyl().

roster %>%
  tabyl(full_time, subject, employee_status, show_missing_levels = FALSE)

# 1. Como crear una función -----

myfuction <- function(){
  x <- rnorm(100, mean=3, sd=2)
  mean(x)
}
?rnorm

#dnorm gives the density, pnorm gives the distribution function, 
#qnorm gives the quantile function, and rnorm generates random deviates.

?ls
ls()
myfuction()
y <- 50

myfuction <- function(x){
  x+5
}

myfuction(y)

dir() #directorio: proyecto y sus elementos

rnorm(1, sd=50)

#operaciones básicas -------

3+5
8/5
4^2
4**2
8%%5 #modulo
sqrt(100)
round(3.1416)
pi
round(pi, 2)

# Matrices ------

?matrix
matrix(c(1,2,3,4,5,6), nrow = 2)
matrix(c(1,2,3,4,5,6), ncol = 2)
matrix(c(1,2,3,4,5,6), ncol = 2, byrow = TRUE)
t(matrix(c(1,2,3,4,5,6), ncol = 2, byrow = TRUE))

#matrix escalar con solo números en la diagonal

diag(c(1,-2,3,-4))

#matriz identidad
diag(3)

#suma
# A+B; A-B (con matrices conformables)

# * multiplica elemento por elemento

#para mult. matrices conformables --> %*%

# transponer --> t(B)

#Sitemas de ecuaciones: 
#5x + 2y - z = 3
#2x + 3y  + 4z = -1
#3x + 4y + 2z = 8

(W <- matrix(c(5,2,3,2,3,4,-1,4,2), nrow=3))
#determinante

det(W) #invertible
(Winv <- solve(W)) #la inversa

(d <- c(3, -1, 8))

#solución

(x <- Winv%*%d)

# This dataset is compiled by the NSW Office of Environment and 
# Heritage contains the enterococci counts in water samples obtained from 
# – you guessed it – Sydney

here()

#beaches <- read_csv("data", "sydneybeaches.csv") #nombre carpeta y luego el del archivo
library(readr)
beaches <- read_csv("data/sydneybeaches.csv")


#o
beaches <- read_csv(here("data", "sydneybeaches.csv")) #nos indica como vienen las columnas

# exploring the data ------

#muchas formas de hacer lo mismo
View(beaches)
dim(beaches)
str(beaches)
glimpse(beaches)

head(beaches, 10)

tail(beaches)
tail(beaches, 10)

summary(beaches)

# skimr

#install.packages("skimr")
library(skimr)

skim(beaches)

# Para graficar ------

# install.packages("ggplot2")
# install.packages("RColorBrewer")#Paletas de colores

library(ggplot2)
library(RColorBrewer)

data("midwest", package = "ggplot2")#se saca base de datos del ggplot

ggplot(midwest)#hoja transparente
ggplot(midwest, aes(x=area, y=poptotal))#rejilla.
options(scipen = 999) #vista menos cientifica en ejes,
ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point() #agrega datos

grafica_1 = ggplot(midwest, aes(x=area, y=poptotal)) + geom_point() +geom_smooth(method = "lm")#se agrega linea regresion con modelo lineal
grafica_1
grafica_1 = ggplot(midwest, aes(x=area, y=poptotal)) + geom_point(col='green') +geom_smooth(method = "lm")#se agreg col verde a puntos
grafica_1
grafica_1 = ggplot(midwest, aes(x=area, y=poptotal)) + geom_point(aes(col=state), size=2) +geom_smooth(method = "lm")
grafica_1 #le dimos color por state --> manera de gráficar 3 variables en 2d
grafica_1+theme(legend.position = "None")#ignora label del cada estado
grafica_2 <- grafica_1 +
 xlim(c(0, 0.1))+
 ylim(c(0, 1000000))#elimina observaciones fuera del rango que yo escoja(elimina datos, regresion diferente)

grafica_2

grafica_3 <- grafica_1 +coord_cartesian(xlim =c(0,0.1), ylim = c(0,1000000)) #hace un zoom, no elimina datos
grafica_3

grafica_4 <- grafica_3 +labs(title="Área vs Población", subtitle = "EE.UU", 
                             y= "Población total", x= "Area", caption = "Monitoria Econometría")
grafica_4


grafica_4 +scale_x_continuous(breaks = seq(0, 0.1, 0.01), labels = letters[1:11])
#qué tan precisos se desean los ejes, colocarle letras a los numeros

grafica_4 + scale_x_continuous(breaks = seq(0, 0.1, 0.01)) + scale_y_continuous(labels = function(x){paste0(x/1000000, "Millón")}) 

# concatenar ----
paste0("Hola", "chicos")
paste("hola", "chicos")

grafica_4 + theme_bw()
grafica_4 + theme_dark() + theme(plot.title = element_text(hjust=0.5))#centra titulo.

grafica_5 = ggplot(midwest, aes(x=area, y=poptotal))+ 
  geom_point(aes(col=state, size=popdensity)) +
                 geom_smooth(method = "lm")
grafica_5 #tenemos graficadas 4 variables 

# Feliz finde chicos-----

# Tao -----
# Demasiado color ciega
# Demasiado ruido, ensordece
# Demasiado condimento, empagala
# Demasiado deseo, entristece
# Demasiado _____, __________



# Adicional ------
#importar datos de una página de internet. 

install.packages("foreign")
library(foreign)

ceolsal1 <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/ceosal1.dta")



# Montecarlo -------

# para comprobar el insesgamiento.

set.seed(123) #semilla para generar números aleatorios

n <- 5000 #Observaciones

r <- 15000 #número de simulaciones (universos)

b1 <- 2
b2 <- 0.7

#error estandar es la estimación de la desviación estándar del error.

su <- 1.8

b1hat <- numeric(r) # entonces tenemos r espacios para las realizaciones de b1
b2hat <- numeric(r)

X <- rnorm(n, mean=4, sd=1) # n números con media 4 --> son fijas por eso fuera del for. 

for(j in 1:r){#repetir para todas las muestras
  u <- rnorm(n, mean=0, sd=su) #los errores con su respectiva desviación estandar
  Y <- b1 + b2*X + u
  #eso era el modelo poblacional --> ahora calculamos el OSL
  bhat <- coefficients(lm(Y~X)) 
  b1hat[j] <- bhat["(Intercept)"] #tambien se puede con [1]
  b2hat[j] <- bhat["X"]
}

hist(b1hat)
hist(b2hat)

mean(b1hat) #estimación del parámetro
mean(b2hat)

var(b1hat) #no tan altas.
var(b2hat)

plot(NULL, 
     main = "Línea de Regresión Poblacional vs líneas OLS",
     xlim = c(0,8),
     ylim = c(0,10),
     xlab = "X",
     ylab = "Y")
#añadimos las lineas  de 15 regresiones

for(j in 1:15) abline(b1hat[j], b2hat[j], col= "gray") #estimadas

#poblacional
abline(b1, b2, col="red")
legend("topleft", c("Poblacional", "Regresiones OLS"),
       lwd = c(2,1),
       col = c("red", "gray"))