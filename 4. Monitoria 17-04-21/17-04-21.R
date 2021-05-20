rm(list = ls())

#regresión multiple -------


library(readxl)


tabla <- read_excel("Tabla1.xlsx")
View(tabla)

(x1<- tabla$x1_t)
x2<- tabla$x2_t
x3<- tabla$x3_t
y<- tabla$y_t

summary(tabla)
modelo<-lm(y~x1+x2+x3-1) #ojo, forzando a pasar por el origen

library(stargazer)

#install.packages("svyglm")
library(car)
stargazer(modelo, type="text") #ojo, forzando a pasar por el origen
?`LinearMethodsList-class`

#prueba de hipotesis
linearHypothesis(modelo, c("x1 = 0"))

linearHypothesis(modelo, c("x1 = 1", "x2 = 1"))



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

# Interpretación de gráficos de regresión ---------

# relación entre el peso del auto (wt) y la cantidad de millas por galón (mpg) 

library(dplyr)
?dplyr
library(ggplot2) 

ggplot(mtcars, aes(x=wt, y=mpg))+
  geom_point()+
  theme_minimal()

#1. Correlación No implica causalidad --> piratas y el calentamiento global
# o Nicolas Cage y el # de ahogados son buenos ejemplos.

#La función geom_smooth se utiliza para agregar la recta de regresión
#Cada punto de la recta: en términos de esperanzas condicionales E[y|x].

ggplot(mtcars, aes(x=wt, y=mpg))+
  geom_point()+
  geom_smooth(method=lm, se=F) + #se --> agrega el intervalo de confianza
  theme_minimal()

#¿y si la relación no fuera lineal?

ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point() +
  geom_smooth(se=FALSE, color="green") +
  theme_minimal()

#Análisis condicional por grupos de variables -------

#es importante siempre condicional la relación a lo interno de las diversas 
#características de nuestra población, esto para evitar incurrir en un análisis 
#erróneo de las relaciones.

#se agregan las opciones Shape (forma del punto) y color para modificar el color 
#a lo interno de los diversos grupos --> según # de cilindros: 

ggplot(mtcars, aes(x=wt, y=mpg, shape=factor(cyl), color=factor(cyl)))+
  geom_point()+
  geom_smooth(method=lm, se=F, fullrange=T, linetype = 1)+ #identifica las pendientes condicionales a los factores
                                #con , fullrange=T se proyectan completas las lineas
  theme_classic()+
  theme(legend.position = "bottom")

?geom_smooth
vignette("ggplot2-specs") #para ver documentación sobre caract. de gráficos

#podemos comparar con la linea de regresión incondicional: 

ggplot(mtcars, aes(x=wt, y=mpg, shape=factor(cyl), color=factor(cyl))) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE) +
  stat_smooth(aes(group=1), method = "lm", se=F) +
  theme_classic()+
  theme(legend.position = "bottom")

# Formas funcionales del modelo de regresión ------

#la no linealidad está sugiriendo la especificación de modelos cuadráticos.
#Mientras que los diferenciales de pendientes en las relaciones observadas a 
#partir de las rectas de regresión en la figura anterior, sugieren interacciones 
#entre las variables que deben modelarse incluyendo interacciones en el modelo.  

# Relación por quintiles -------
#A quintile is a statistical value of a data set that represents 20% of a given 
#population, so the first quintile represents the lowest fifth of the data 
#(1% to 20%); the second quintile represents the second fifth (21% to 40%) 
#and so on.

ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_jitter(width = 0.5, size = 1)+
  stat_quantile(quantiles = c(0.05, 0.5, 0.95))+
  theme_classic()

?geom_jitter

#También podemos segmentar el análisis condicional a los diversos sub grupos.
#Clasificación por clusters
#La función stat_ellipse permite la clasificación de grupos

ggplot(mtcars, aes(x=wt, y=mpg, shape=factor(cyl), color=factor(cyl))) +
  geom_point() +
  theme_classic() +
  theme(legend.position = "bottom")+
  stat_ellipse(type="norm")+
  scale_color_brewer(palette="Dark2")

#Un parentesis en el caso del gráfico de dispersión en series temporales ----
#relacionado con el tema de estacionariedad visto en clase:

# el caso de trabajar con series temporales, es que la correlación se hace bajo 
#supuesto de independencia en las series
#por esto, en el caso de series temporales se suelen estudiar la asociación entre 
#alguna transformación de las series
# tal como la data de crecimiento o la primera diferencia en logaritmo. 
#Series que se suponen estacionales.

#install.packages("ggfortify")
library(ggfortify)
EuStockMarkets[,"DAX"] %>%
  autoplot() +
  theme_minimal()

log(EuStockMarkets[,"DAX"]) %>%
      autoplot() +
      theme_minimal()

diff((EuStockMarkets[,"DAX"]), differences=1) %>%
  autoplot() +
  theme_minimal()

diff(log(EuStockMarkets[,"DAX"]), differences=1) %>%
  autoplot() +
  theme_minimal()


# Visualizar rápidamente correlaciones entre variables -------
#install.packages("GGally")
library("GGally")

tasaData <- EuStockMarkets %>% as.data.frame() %>%
  
  mutate(
    across(everything(),
           ~((./dplyr::lag(.)-1)*100),
           .names= "tc_{.col}"
    )) %>%
  select(starts_with("tc"))

head(tasaData)

tasaData %>%
  ggpairs()

# Relación M1 con ingreso y tasa de interés------

library(readxl)

data <- read_excel("Tabla2.xlsx")


y <- data$`Y (billions)`
m <- data$`m (billions)`
i <- data$`i (%)`

View(data)
modelo1 <- lm(m~y+i)
#summary(modelo1)
#library(stargazer)
stargazer(modelo1, type="text")

vcov(modelo1)

Prediccion1 <- 89.777354+0.135940*(1000)-2.577072*(12)
Prediccion1
Prediccion2 <- 89.777354+0.135940*(2000)-2.577072*(6)
Prediccion2

betas <- coef(modelo1)

(Prediccion1 <- betas[1]+betas[2]*1000+betas[3]*12) #más sencillo

#modelo log - log -----

library(readxl)

ln_y<- data$`Ln(Y (billions))`
ln_m<- data$`Ln(m (billions))`
ln_i<- data$`Ln(i (%))`

modelologlog <- lm(ln_m~ln_y+ln_i)
stargazer(modelologlog, type="text")
vcov(modelologlog)

#o podemos hacer la transformación directamente en r
?log
lny <- log(y) 
lni <- log(i)
lnm <- log(m)

modelo_log <- lm(lnm ~ lny + lni)
stargazer(modelo_log, type = "text")
(coef_log <- coef(modelo_log))

(ln_Prediccion1 <- coef_log[1]+coef_log[2]*log(1000)-coef_log[3]*log(12))

ln_Prediccion2 <- 0.57473+0.70913*log(2000)-0.05332*log(6)
ln_Prediccion2

exp(ln_Prediccion1)
exp(ln_Prediccion2)

#introducción a la herramienta swirl ------
# The swirl package turns the R console into an interactive learning environment.

#install.packages("swirl")
packageVersion("swirl")

library(swirl)

#swirl offers a variety of interactive courses, but for our purposes, 
#you want the one called R Programming

install_from_swirl("R Programming")
#to start swirl:

swirl()

#For more information on swirl, visit swirlstats.com
Luis

#type bye() to exit and save your progress.
# | -- Typing skip() allows you to skip the current question.
# | -- Typing play() lets you experiment with R on your own; swirl will ignore what you do...
# | -- UNTIL you type nxt() which will regain swirl's attention.
# | -- Typing bye() causes swirl to exit. Your progress will be saved.
# | -- Typing main() returns you to swirl's main menu.
# | -- Typing info() displays these options again.

#diviertanse :3
main()

#?tapply

bye()
