library(tidyverse)
library(tmap)
library(sf)
tmap_mode('view')

#Leer el archivo metadata_estaciones_agrometAPI.csv (paso 1)
info_data <- read.csv('metadata_estaciones_agrometAPI.csv', sep = ";")

#Eliminar columnas x del dataframe (paso 2)
info_data <- info_data%>% select (-one_of ('X'))

#convertir variables str a numericas (latitud y longitud) (paso 3)
info_data$latitud <- as.numeric(gsub(",", ".", info_data$latitud))
info_data$longitud <- as.numeric(gsub(",", ".", info_data$longitud))

class(info_data)
str(info_data)
print(info_data)

#Leer el archivo data_estaciones_agrometAPI.rds
data <- readRDS('data_estaciones_agrometAPI.rds')
class(data)
str(data)

#Mapa de ubicación de las estaciones
info_data |> 
  st_as_sf(coords = c('longitud','latitud'),crs=4326) ->dtmap

tm_shape(dtmap) +
  tm_markers(clustering = FALSE)

#Seleccionar seed y las estaciones para cada alumno
set.seed(987)
estaciones <- unique(data$station_id)

l <- lapply(1:5,sample,x=estaciones,size=10)
names(l) <- paste0('Alumno_',1:5)
l
class(l)
#==================================================Ejercicio 1==============================================#
#De los set de datos original debe filtrar las estaciones asignadas
estaciones_asignada <- l[["Alumno_5"]]
datos_filtrados <- data %>% filter(station_id %in% estaciones_asignada)
str(datos_filtrados)
#==================================================Ejercicio 2==============================================#
#Cree un script que permita calcular el promedio de las variables temp_promedio_aire, temp_minima y temp_maxima; 
#para todas las estaciones asignadas y todo el año 2021. Excluya del cálculo los valores no disponibles (NA).
# Calcular los promedios excluyendo valores NA
promedio_temp_promedio_aire <- mean(datos_filtrados$temp_promedio_aire, na.rm = TRUE)
promedio_temp_minima <- mean(datos_filtrados$temp_minima, na.rm = TRUE)
promedio_temp_maxima <- mean(datos_filtrados$temp_maxima, na.rm = TRUE)
# Mostrar los resultados
cat("Promedio de temp_promedio_aire:", promedio_temp_promedio_aire, "\n")
cat("Promedio de temp_minima:", promedio_temp_minima, "\n")
cat("Promedio de temp_maxima:", promedio_temp_maxima, "\n")

#==============================================Ejercicio 3 y 4==============================================#
#Escriba un script que permita calcular la suma de la precipitación anual para cada una de las estaciones
# Agrupar los datos por estación y año, y luego calcular la suma de precipitación para cada grupo
#Cree un data.frame con el resultado del ejercicio anterior, 
#debe tener dos columnas, una con el identificador de la estación y otra con la suma anual de precipitación.
suma_precipitacion_anual <- datos_filtrados %>%
  group_by(station_id) %>%
  summarize(suma_precipitacion = sum(precipitacion_horaria, na.rm = TRUE))

print(suma_precipitacion_anual)
str(suma_precipitacion_anual)

#==================================================Ejercicio 5==============================================#
#Cree un script que permita calcular el promedio de las variables temp_promedio_aire, temp_minima y temp_maxima;
#para cada una de las estaciones asignadas y todo el año 2021. Excluya del cálculo los valores no disponibles (NA).
#Vea la ayuda de la función aggregate

# Crear una función para calcular promedio excluyendo NA
mean_without_na <- function(x) {
  mean(x, na.rm = TRUE)
}

# Calcular promedios para cada variable y estación
resultados_promedios <- aggregate(. ~ station_id, datos_filtrados[, c("station_id", 
                                                                      "temp_promedio_aire", 
                                                                      "temp_minima", 
                                                                      "temp_maxima")], FUN = mean_without_na)

# Mostrar los resultados
print(resultados_promedios)

#==================================================Ejercicio 6==============================================#
#Cree un scrpt para dividir el data.frame en una lista donde cada elemento de la lista debe corresponder 
#al subset de datos de cada estación. Busque ayuda sobre la función split

# Dividir el data.frame en una lista por estación
lista_estaciones <- split(datos_filtrados, datos_filtrados$station_id)

# Mostrar la lista de estaciones (por ejemplo, para la estación 106)
print(lista_estaciones)

#==================================================Ejercicio 7==============================================#
#Cree un script para hacer un grafico de tipo boxplot 
#para cada una de las estaciones respecto a las variables temperatura promedio

# Filtrar los datos de temperatura promedio
datos_temperatura_promedio <- select(datos_filtrados,station_id, temp_promedio_aire)

# Crear el gráfico de tipo boxplot
ggplot(datos_temperatura_promedio, aes(x = as.factor(station_id), y = temp_promedio_aire)) +
  geom_boxplot() +
  labs(x = "Estación", y = "Temperatura Promedio") +
  ggtitle("Boxplot de Temperatura Promedio por Estación") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = estaciones_asignada)  # Etiquetas de estaciones
#==================================================Ejercicio 8==============================================#
#Similar al ejercicio anterior, cree un script para hacer un grafico de tipo scatterplot 
#para fecha_hora vs temp_promedio_aire para cada una de las estaciones.

# Filtrar los datos de temperatura promedio y fecha_hora
datos_temperatura_fecha <- datos_filtrados %>%
  select(station_id, fecha_hora, temp_promedio_aire)

# Crear un gráfico scatterplot para cada estación en subparcelas
ggplot(datos_temperatura_fecha, aes(x = fecha_hora, y = temp_promedio_aire)) +
  geom_point() +
  labs(x = "Fecha y Hora", y = "Temperatura Promedio") +
  ggtitle("Scatterplot de Temperatura Promedio por Estación") +
  theme_minimal() +
  facet_wrap(~station_id, scales = "free_x", ncol = 2)

#==================================================Ejercicio 9==============================================#
#Escriba un script que permita extraer el mes de enero de cada una de las estaciones para las variables
#c("station_id", "fecha_hora", "temp_promedio_aire", "precipitacion_horaria", "humed_rel_promedio",
#"presion_atmosferica"). Utilice la lista del ejercicio 5.
extraer_enero <- function(estacion) {
  enero <- estacion %>%
    filter(month(fecha_hora) == 1)
  return(enero)
}

datos_enero <- lapply(lista_estaciones, extraer_enero)

str(datos_enero)

#==================================================Ejercicio 10==============================================#
# Verificar si hay datos para los 31 días de enero de 2021
verificar_dias_enero <- function(estacion) {
  # Crear un vector con los días del mes de enero
  dias_enero <- 1:31
  # Extraer los días de enero presentes en los datos
  dias_presentes <- estacion %>%
    filter(month(fecha_hora) == 1) %>%
    pull(day(fecha_hora))
  # Verificar si todos los días de enero están presentes
  dias_faltantes <- setdiff(dias_enero, dias_presentes)
  if (length(dias_faltantes) == 0) {
    mensaje <- "Todos los días de enero están presentes."
  } else {
    mensaje <- paste("Faltan los días:", paste(dias_faltantes, collapse = ", "))
  }
  return(mensaje)
}

# Aplicar la función a cada estación en la lista de datos de enero
resultados_verificacion <- lapply(datos_enero, verificar_dias_enero)

# Mostrar los resultados para cada estación
for (i in 1:length(resultados_verificacion)) {
  cat("Estación ID:", names(resultados_verificacion)[i], "->", resultados_verificacion[[i]], "\n")
}