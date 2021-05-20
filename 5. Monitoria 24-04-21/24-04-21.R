rm(list = ls())
#if-else --> logical conditions
#for --> loop
#whiles --> ejecutar hasta que una condición se cumple

# if - else -------
x <- 20
if(x<3){
  y <- 10
}else if (x <10){
   y<-0
}else {
   y <- 5
 }

x ; y

#lo anterior es igual a: 

y <- if (x<3){
  10
}else{
  0
}
x;y

#o se puede plantear el if solo sin condición para el else

#for ------
#el más común

for (v in 1:10){
  print(v)
}

x <- c("a", "b", "c", "d")

for(i in x){
  print(i)
}
?seq_along #función para crear secuencias
seq_along(x)

for (i in seq_along(x)){
  print(x[i])
}

#para ahorrarnos los corchetes
for (i in x) print(i)

#for can be nested

x <- matrix(1:6, 2, 3)

for (i in seq_len(nrow(x))) {
  for (j in seq_len(ncol(x))){
    print(x[i,j])
  }
}

#while -----
count <- 0

while(count < 22){
  print(count)
  count <- count +1
} #ser cuidadosos con que si va a llegar a la condición sino podemos tener problemas

?rbinom()

count <- 25
while(count> 20 && count <=30){
  print(count)
  coin <- rbinom(1, 1, 0.5) #moneda justa
  
  if(coin == 1){ #random walk
    count <- count+1
  }else{
    count <- count-1
  }
}

#visualizando datos de COVID-19 ------

# ver archivo Tidyverse cheat sheet

# how quickly did the virus spread across the globe? And, can we see any effect 
#from country-wide policies, like shutdowns and quarantines?

#Johns Hopkins University Center for Systems Science and Engineering created a 
#publicly available data repository to consolidate this data from sources like 
#the WHO. 

#visualize COVID-19 data from the first several weeks of the outbreak to see at 
#what point this virus became a global pandemic.
#hasta March 17, 2020

# Load the readr, ggplot2, and dplyr packages
library(readr)
#install.packages("")
library(ggplot2, dplyr)

# Read datasets/confirmed_cases_worldwide.csv into confirmed_cases_worldwide

#install.packages("coronavirus")

library(coronavirus)

update_dataset()

covid19_df <- refresh_coronavirus_jhu()
head(covid19_df)
#Summary of the total confrimed cases by country (top 20):

library(dplyr)

summary_df <- coronavirus %>% 
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases)

library(tidyr) #en el último día

coronavirus %>% 
  filter(date == max(date)) %>%
  select(country, type, cases) %>%
  group_by(country, type) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type,
              values_from = total_cases) %>%
  arrange(-confirmed)

#plotting the total cases

library(plotly)

coronavirus %>% 
  group_by(type, date) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type, values_from = total_cases) %>%
  arrange(date) %>%
  mutate(active = confirmed - death - recovered) %>%
  mutate(active_total = cumsum(active),
         recovered_total = cumsum(recovered),
         death_total = cumsum(death)) %>%
  plot_ly(x = ~ date,
          y = ~ active_total,
          name = 'Active', 
          fillcolor = '#1f77b4',
          type = 'scatter',
          mode = 'none', 
          stackgroup = 'one') %>%
  add_trace(y = ~ death_total, 
            name = "Death",
            fillcolor = '#E41317') %>%
  add_trace(y = ~recovered_total, 
            name = 'Recovered', 
            fillcolor = 'forestgreen') %>%
  layout(title = "Distribution of Covid19 Cases Worldwide",
         legend = list(x = 0.1, y = 0.9),
         yaxis = list(title = "Number of Cases"),
         xaxis = list(title = "Source: Johns Hopkins University Center for Systems Science and Engineering"))

#Plot the confirmed cases distribution by counrty with treemap plot:

conf_df <- coronavirus %>% 
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases) %>%
  mutate(parents = "Confirmed") %>%
  ungroup() 

plot_ly(data = conf_df,
        type= "treemap",
        values = ~total_cases,
        labels= ~ country,
        parents=  ~parents,
        domain = list(column=0),
        name = "Confirmed",
        textinfo="label+value+percent parent")

## Condición Marshall Lerner para Colombia, Luis Castellanos ------

library(readxl)

data <- read_excel("Datos para trabajo final.xlsx")
View(data)
# Colombia es uno de los países que puede practicar la devaluación 
#competitiva?, 
#depreciar su moneda resulta favorable para su balanza comercial?

#XN en términos de TCR, Ylocal y Indice ponderado de Y de socios

XN <- as.numeric(data$`Índice Exportaciones Netas (XN)`)
TRM <- as.numeric(data$`TRM empalmada de datos promedio por meses.`)
IPI <- as.numeric(data$`Índice de Producción Real de la Industria Colombiana Base 2014 (PIB)`)
Y_ext <- as.numeric(data$`Ponderado Fisher del Y* Extranjero Agregado`)

XN
TRM
IPI
Y_ext

Regre <- lm(XN~ IPI+TRM+Y_ext)
library(stargazer)
stargazer(Regre, type="text")

library(ggplot2)
ggplot(Regre, aes(x=XN, y=TRM))+
  geom_line()+
  theme_classic()

ln_XN <- log(XN)
ln_TRM <- log(TRM)
ln_IPI <- log(IPI)
ln_Y_ext <- log(Y_ext)

Regre_M_L <- lm(ln_XN~ ln_IPI+ln_TRM+ln_Y_ext)
stargazer(Regre_M_L, type="text")

ggplot(Regre, aes(x=ln_XN, y=ln_TRM))+
  geom_line()+
  theme_classic()



#Reestimamos con el modelo depurado
Regre_dep <- lm(ln_XN ~ ln_TRM + ln_Y_ext)
stargazer(Regre_dep, type="text")

library(lmtest)
coeftest(Regre_dep)
#estimamos los intervalos de confianza. 
confint(Regre_dep)

#Reset para correcta especificación --> Test de Ramsey
reset(Regre_dep)
resettest(Regre_dep)# mismo resultado, pero R-Studio sugiere usar 

#Hacemos prueba para probar si existe multicolinealidad entre las variables. 
cor(cbind(ln_TRM,ln_Y_ext))
#adicionalmente se estiman las regresiones auxiliares entre las variables explicativas, si el R^2 es mayor al del modelo original, entonces eso nos indicar?a un problema de multicolinealidad. 


# Aqui vamos ---------------
# summary(Regre_dep)$r.squared
# R_cuadrado_TRM <- summary(lm(ln_TRM~ln_Y_ext))$r.squared
# R_cuadrado_Y_ext <- summary(lm(ln_Y_ext~ln_TRM))$r.squared
# R_cuadrado_TRM
# R_cuadrado_Y_ext

# Factor de inflación de la varianza (problemas si >5)
#vif=1/(1-R^2)
install.packages("faraway")
library(faraway)
vif(Regre_dep) 

#Como el valor para ambas variables es menor a 5 (relativamente bajo), 
#se concidera que no hay problema de multicolinealidad. 

#Se procede a hacer pruebas de cambio estructural. 

install.packages("strucchange")
library(strucchange)
#Proceso de Fluctuaci?n Emp?rico que se basa en residuos recursivos de MCO
cusum.prueba = efp(ln_XN~ln_TRM+ln_Y_ext,type = "OLS-CUSUM")
plot(cusum.prueba)    #Gr?fico del proceso con bandas de rechazo
##Si la grafica se sale de la banda o se acerca mucho en un punto entonces puede haber cambio estructural en ese punto y en los que presentan ese comportamiento

#Test de Chow (Ho:Estabidlidad param?trica, es decir, no C.Estructural)
##Aqui tengo que saber DONDE es el cambio estructural
##Hay que ordenar los datos
sctest(ln_XN~ ln_TRM+ln_Y_ext , type="Chow",point=40)# no presenta cambio estructural
#Podemos si hay cambio estructural en el intervalo identificado de 3 formas. 
sctest(ln_XN~ ln_TRM+ln_Y_ext , type="Chow", from=14,to=23)
sctest(ln_XN~ ln_TRM+ln_Y_ext, type = "Chow")
sctest(ln_XN~ ln_TRM+ln_Y_ext, type = "Chow", from = 0.3, to=0.5)
##Si pvalor<nivel de significancia, rechazo Ho concluyo que no hay cambio estructural

#Procedemos a reestimar el modelo incluyendo variable dummy
d <- as.vector(cbind(rep(0,45)))
for (i in 14:23) {
  d[i] <- 1
}
d
TRM
dln_TRM <- as.vector(cbind(rep(0,45)))
for (i in 14:23) {
  dln_TRM[i] <- TRM[i]
}
dln_TRM
dln_Y_ext <- as.vector(cbind(rep(0,45)))
for (i in 14:23) {
  dln_Y_ext[i] <- Y_ext[i]
}
dln_Y_ext
Regre_dep_dummy <- lm(ln_XN~d+ln_TRM+dln_TRM+ln_Y_ext+dln_Y_ext)
stargazer(Regre_dep_dummy, type="text")
#summary(Regre_dep_dummy) #ninguna de los par?metros introducidos es 
#significativo (el cambio estructural es demasiado peque?o)

#Prueba de heterocedasticidad: 

#HETEROCEDASTICIDAD:
install.packages("car")
library(car)
# grafico de dispersion de residuos studentizados

spreadLevelPlot(Regre) #continuamos trabajando con el modelo 
#depurado ya que las variables dummy no eran relevantes.
##Si el grafico de dispersion sigue tendencia creciente o 
#decreciente la varianza de los errores no es constante
##Existiria heterocedasticidad (posiblemente)-> 

#Prueba Breush-Pagan(Ho:Homocedasticidad)
bptest(Regre_dep)
#Prueba de varianza no constante (Ho:Homocedasticidad)
ncvTest(Regre_dep)
#Prueba Goldfeld-Quand (Ho:Homocedasticidad)
gqtest(Regre_dep)

#AUTOCORRELACION:
#Durbin Watson test (Ho:No autocorrelaci?n de 1er orden)
dwtest(Regre_dep)

#Prueba Breush-Godfrey (Ho:No autocorrelaci?n de orden p)
bgtest(Regre_dep)

#Gr?fico de autocorrelaci?n
acf(Regre_dep$residuals)
##Si se sale de la banda hay correlacion entre el errort y 
#el errort-1
##Este caso es de orden 2

#Correccion a autocorrelaci?n con Estimaci?n Robusta

install.packages("MASS")
library(MASS)
reg.rob=rlm(ln_XN~ln_TRM+ln_Y_ext)
library(stargazer)
stargazer(reg.rob, type="text")
stargazer(Regre_dep, type="text")
# summary(reg.rob)
# summary(Regre_dep)

#NORMALIDAD
#Jarque bera (Ho:  Normalidad)
install.packages("tseries")
library(tseries) 
jarque.bera.test(reg.rob$residuals)

# shapiro-wilk (Ho: Normalidad)
shapiro.test(reg.rob$residuals)

install.packages("lmtest")
library(lmtest)
coeftest(reg.rob)
#estimamos los intervalos de confianza.

coeficientes <- as.vector(coef(reg.rob))
varianzas <- as.vector(diag(vcov(reg.rob)))
varianzas
intervalos <-matrix(nrow = 3, ncol = 2)
for (i in 1:3) {
  intervalos[i,] <- c(coeficientes[i]-sqrt(varianzas[i]*qt(0.975,42)),coeficientes[i]+sqrt(varianzas[i]*qt(0.975,42)))
}
intervalos

# Datos originales

base = data.frame(ln_XN,ln_TRM,ln_Y_ext)   # se debe crear un data.frame con datos originales

# Estimaci?n del modelo
modelo = reg.rob
summary(modelo)

# PREDICCI?N POR DENTRO (Y estimados)------------------------------------------------
# 

lnXN_estimado = predict(modelo)
cbind(ln_XN,lnXN_estimado)   # comparaci?n

# Gr?fico para una serie de tiempo
yt = ts(ln_XN,start = 2015,frequency = 12) # definir periodicidad (en este ejemplo es trimestral desde 2000)
yt.est = ts(lnXN_estimado,start = 2015,frequency = 12) # la predicci?n por dentro tiene la misma peridicidad
ts.plot(yt,yt.est,col=1:2)

#-------------------------------------------------------------
# PREDICCI?N POR FUERA

# Crear bases de datos para predicci?n
# es muy IMPORTANTE que las variables de la nueva base tengan los mismos nombres de las ex?genas

# alternativa1: base con valores de X hipot?ticos para el proximo a?o. 
LN_TRM=seq(from=2893.22,by=25, to=3168.22)
LN_TRM
LN_Y_EXT=seq(from=54.16, by=0.4, to=58.4)
LN_Y_EXT
?data.frame
Xo = data.frame(TRM = LN_TRM, Y_EXT =LN_Y_EXT)
Xo

# estimaci?n alternativa 1  (series de tiempo)
y.pron = predict(modelo,newdata = Xo)
yt.pron = ts(y.pron,start = c(2018,12),frequency = 12) #definir priodicidad futura
ts.plot(yt,yt.est,yt.pron+0.2,col=1:3)
summary(modelo)


