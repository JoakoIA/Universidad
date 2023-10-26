# Parte 1: R base
# Joaquin Gutierrez
# 1.
print("Indique cuales son los tipos de vectores que existen en R y de un ejemplo de cada uno.")
print(paste("Existen 5 tipos de vectores, los cuales son los vectores de tipo caracter,",
            " numerico, entero, complejos y logicos."))

# 2.
print("De un ejemplo de un vector atómico nombrado.")
print(paste("Un vector atómico nombrado es el siguiente:"))
vectorAtomico <- c(1,2,3,4,5)
print(vectorAtomico)

# 3.
print("¿Cómo se almacenan los nombre de un vector atómico nombrado?")
print(paste("Los nombres de un vector atómico nombrado se almacenan de la siguiente manera:"))
names(vectorAtomico) <- c("a","b","c","d","e")
print(vectorAtomico)

# 4.
#Explique por qué al hacer la siguiente operación en R no se genera un error y por qué sí debería
#(considerando el funcionamiento en otros lenguajes), además explique que es lo que hace la linea de código.
iris$col <- rnorm(10)
print(paste("La linea de codigo anterior no genera un error porque se esta agregando una nueva columna al dataframe iris,",
      "la cual se llama col y se le esta asignando un vector de 10 numeros aleatorios."))
print(paste("Si se intentara agregar una nueva columna al dataframe iris, pero esta columna no tuviera la misma cantidad de filas",
        "que el dataframe, entonces si se generaria un error."))

# 5.
#Explique que es lo que hace la siguiente operación.
print(rnorm(5)+rnorm(10))
print(paste("La linea de codigo anterior genera un vector de 10 numeros aleatorios,",
      "luego genera un vector de 5 numeros aleatorios y luego suma ambos vectores."))
print(paste("Al sumar ambos vectores, se genera un vector de 10 numeros aleatorios,",
        "ya que el vector de 5 numeros aleatorios se repite 2 veces para poder sumarse con el vector de 10 numeros aleatorios."))

# 6.
#Reescriba el siguiente código en una sóla linea.
v <- vector()
for (i in 1:10){
  v[i] <- log(i)
}
print(v)
print(paste("Asi queda el codigo anterior escrito en unasola linea:"))
w <- log(1:10)
print(w)

# 7.
#Aplique la función f que se define en el código de abajo a cada elemento del vector t. Haga un gráfico de y vs x.
f <- function(t) {
  x <- sin(t) * (exp(cos(t)) - 2 * cos(4*t) - sin(t/12)^5) 
  y <- cos(t) * (exp(cos(t)) - 2 * cos(4*t) - sin(t/12)^5) 
  return(list(x,y))
}

t <- seq(0,5,length.out = 1000)

plot(f(t)[[1]],f(t)[[2]])

# 8.
#Aplique una función con apply a cada columna del data.frame airquality para que calcule la raiz cuadrada del promedio.
apply(airquality,2,function(x) sqrt(mean(x)))

# 9.
#Cree una función para recorrer la lista l y que haga los gráficos (puntos) de mpg vs hp.
l <- mtcars |> 
  split(mtcars$cyl)

lapply(l,function(x) plot(x$mpg,x$hp))

# 10.
#Convierta las siguientes funciones anidadas al dialecto con pipes utilizando el pipe nativo de R (|>).
# sin(sqrt(log(x)))
x |> log() |> sqrt() |> sin()

# apply(apply(airquality,1,`[`, c(1,3)),1,mean,na.rm = TRUE)
airquality |> apply(1,`[`, c(1,3)) |> apply(1,mean,na.rm = TRUE)

# summary(lm(Sepal.Width~Sepal.Length,iris))$r.squared
iris |> lm(Sepal.Width~Sepal.Length,.) |> summary() |> r.squared

#Parte 2: Tidyverse 
library(tidyverse)

# 1.
print("¿La siguiente tabla corresponde a un set de datos ajustado (tidy)? ¿Por qué?")
print(paste("Si, ya que cada columna representa un variable diferente y cada fila corresponde a una observacion ",
    "unica"))

# 2.
#Reescriba el siguiente codigo que está escrito en el 
#dialecto de pipe usando el pipe de {magrittr} (%>%) a su versión con el pipe nativo de R (|>).
#library(magrittr)
iris %>% 
  subset(Species == 'versicolor') %>% 
  .$Petal.Length %>% 
  mean(na.rm = TRUE)

#pipe R base
mean(iris[iris$Species == 'versicolor', 'Petal.Length'], na.rm = TRUE) 

iris |>
  subset(Species == 'versicolor') |>
  subset(select = c("Petal.Length")) |>
  mean(na.rm = TRUE)

# 3.
#Existe alguna diferencia entre los dos script que aparecen a continuación. 
#En caso de haber ¿Cuál? (más allá de que uno sea data.frame y otro tibble)
#Script 1
iris[,'Sepal.Width']

#Script 2
iris_tbl <- as_tibble(iris)
iris_tbl[,'Sepal.Width']

print(paste("La diferencia es que en el primer script se obtiene un vector con todos ",
    "los valores de la columna Sepal.Width, mientras que en el segundo script se obtiene ",
    " un tibble de una columna con cada file representando a un valor del vector."))

# 4.
#El set de datos corresponde a los datos desde el día de ayer para dos estaciones climáticas de la red agromet. Realice lo siguiente: 
# 4.1
# Cargue el archivo csv directamente a un objeto tibble con nombre data.
data <- as_tibble(read.csv("GitHub/Universidad/R/datos_estacion_114.csv"))
# 4.2
#Haga explicitos los valores NA de todas las variables climáticas en el sentido de que tengan las mismas fecha_hora. 
#¿Cuántos observaciones (horas) con valores NA tiene data?
data |> 
  group_by(station_id) |> 
  summarise(n_distinct(fecha_hora))

combined_data <- data %>% 
  filter(station_id ==114) %>% 
  slice(1:min(26,n())) %>% 
  bind_rows(data %>% 
              filter(station_id == 314) %>% 
              slice(1:min(33,n())))

data <- data %>% 
  distinct(station_id) %>% 
  bind_rows(combined_data) %>% 
  complete(station_id, nesting(fecha_hora), fill = list(valor = NA))

  
# 4.3
#Convierta en formato largo data considerando todas las variables climáticas.
#Asigne el nombre variables a la columna con los nombres de varables 
#y el nombre valor a la columna que contiene los valores.
data <- data |> 
  pivot_longer(cols = c("temp_promedio_aire","precipitacion_horaria","humed_rel_promedio","temp_minima","temp_maxima"),names_to = "variables",values_to = "valor")

# 4.4
# Reemplace los valores NA de cada variable climática por el valor -999.
data <- data |> 
  mutate(valor = ifelse(is.na(valor),-999,valor))

# 4.5
data <- data |> 
  pivot_wider(names_from = variables,values_from = valor)

# 4.6
# Transforme el set de datos resultante en una tabla anidada para cada station_id, 
#que contenga las variables de fecha_hora y climáticas
data <- data |> 
  group_by(station_id) |> 
  nest()

# 4.7
# Guarde el set de datos resultante como clase tibble en formato RDS. Con el nombre datos_ejercico_24.rds
data |> 
  write_rds("GitHub/Universidad/R/datos_ejercicio_24.rds")

# 5
library(readr)
metadata2 <- read_rds("GitHub/Universidad/R/info_agvAPI.rds")
data2 <- read_rds("GitHub/Universidad/R/data_agvAPI_estacion_110.rds")

metadata2 <- metadata2 %>% 
  filter(serial ==  "z6-10092")
