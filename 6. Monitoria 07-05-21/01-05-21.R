rm(list = ls())

# Monitoria 08/05/21 Econometría 1, Luis Castellanos --------

install.packages("gapminder")
library(gapminder) #base de datos con indicadores socioeconómicos de todo el mundo

library(dplyr)

gapminder
#formato tibble
#filter() --> ver solamente una subsección

gapminder %>%
  filter(year == 2007) #tenemos 142 países.

gapminder %>%
  filter(country == "Colombia") #no tenemos los datos para todos los años
#podemos introducir dos condiciones a la vez.

gapminder %>%
  filter(country == "Colombia", year == 2007)

#arrange se usa para organizar de manera ascendente

gapminder %>%
  arrange(gdpPercap)
#para organizarlo descendentemente podemos usar desc()

gapminder %>%
  arrange(desc(gdpPercap))

gapminder %>%
  filter(year == 2007)%>%
  arrange(desc(gdpPercap))

gapminder %>%
  mutate(gdp = gdpPercap*pop) %>%
  filter(year == 2007)%>%
  arrange(desc(gdp))

#para bono ------

gapminder%>%
  filter(year == 2007)%>%
  mutate(lifeExpMonths = 12*lifeExp)%>%
  arrange(desc(lifeExpMonths))

gapminder_1952 <- gapminder %>%
  filter(year == 1952)

library(ggplot2)

ggplot(gapminder_1952, aes(x = gdpPercap, y = lifeExp)) +
  geom_point()

ggplot(gapminder_1952, aes(x = pop, y = gdpPercap)) +
  geom_point()

#para 2007
gapminder%>%
  filter(year == 2007)%>%
  ggplot(aes(x = gdpPercap, y = lifeExp))+
  geom_point()

#podemos usar escala logaritmica -------
gapminder%>%
  filter(year == 2007)%>%
  ggplot(aes(x = gdpPercap, y = lifeExp))+
  geom_point() +
  scale_x_log10()


gapminder%>%
  filter(year == 2007)%>%
  ggplot(aes(x = gdpPercap, y = lifeExp, color = continent))+
  geom_point() +
  scale_x_log10()

gapminder%>%
  filter(year == 2007)%>%
  ggplot(aes(x = gdpPercap, y = lifeExp, color = continent, size = pop))+
  geom_point() +
  scale_x_log10()

#podemos dividir los continentes en diferentes gráficos:
#faceting --------


gapminder%>%
  filter(year == 2007)%>%
  ggplot(aes(x = gdpPercap, y = lifeExp))+
  geom_point() +
  scale_x_log10() +
  facet_wrap(~ continent)

#Bono ----

# Create a scatter plot of the gapminder data:
# Put GDP per capita (gdpPercap) on the x-axis and life expectancy (lifeExp) on
#the y-axis, with continent (continent) represented by color and population (pop)
#represented by size.
# Put the x-axis on a log scale
# Facet by the year variable

ggplot(gapminder, aes(x= gdpPercap, y=lifeExp, color=continent, size=pop))+
  geom_point()+
  scale_x_log10()+
  facet_wrap(~ year)

#summarize ------
#permite resumir información de manera personalizada y guardarla en columna

gapminder %>%
  filter(year == 2007)%>%
  summarize(meanLifeExp = mean(lifeExp))

gapminder %>%
  filter(year == 2007)%>%
  summarize(meanLifeExp = mean(lifeExp), totalPop=sum(pop))

#igual se puede con max, min, mean, median, etc

#podemos hacer el análisis para varios años con group_by

(by_year <- gapminder %>%
  group_by(year)%>%
  summarize(meanLifeExp = mean(lifeExp), totalPop=sum(pop)))

gapminder %>%
  group_by(continent)%>%
  summarize(meanLifeExp = mean(lifeExp), totalPop=sum(pop))

(by_year_continent <- gapminder %>%
  group_by(year, continent)%>%
  summarize(meanLifeExp = mean(lifeExp), totalPop=sum(pop)))

gapminder%>%
  filter(year == 1957)%>%
  group_by(continent)%>%
  summarize(medianLifeExp=median(lifeExp), maxGdpPercap=max(gdpPercap))

ggplot(by_year, aes(x= year, y=totalPop))+
  geom_point()

ggplot(by_year, aes(x= year, y=totalPop))+
  geom_point()+
  expand_limits(y =0) #se especifica que se quiere que el eje empiece en 0

ggplot(by_year_continent, aes(x=year, y=totalPop, color=continent))+
  geom_point()+
  expand_limits(y=0)

#lineplots -------

ggplot(by_year_continent, aes(x=year, y=totalPop, color=continent))+
  geom_line()+
  expand_limits(y=0)

#barplots -----
# Summarize the median gdpPercap by continent in 1952
by_continent <- gapminder%>%
  filter(year == 1952)%>%
  group_by(continent)%>%
  summarize(medianGdpPercap=median(gdpPercap))

# Create a bar plot showing medianGdp by continent
ggplot(by_continent, aes(continent, medianGdpPercap))+
  geom_col()

#histograma para mostrar distribuciones -----

ggplot(gapminder_1952, aes(x=lifeExp))+
  geom_histogram()

ggplot(gapminder_1952, aes(x=lifeExp))+
  geom_histogram(binwidth = 5) #toma ventana de 5 años

?geom_histogram

#boxplot --> permite comparar distribución (como en hist) de dif. poblaciones -----
gapminder%>%
  filter(year == 2007)%>%
ggplot(aes(x=continent, y=lifeExp))+
  geom_boxplot()

#linea negra es la media de cada distr.
#caja contiene del 25% al 75% de la distribución 
#las lineas representan el resto de la dist. 
# pero los puntos negros representan outliers. 

# Repeat, next, break ------

#repeat siempre debe ir con break, siempre!

#next se usa cuando se quiere saltar una iteración

for(i in 1:100) {
  if(i <= 20){
    ## saltar primeras 20 iteraciones
    next
  }
  print(i)
}

