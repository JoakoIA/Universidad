library(tidyverse)
library(tmap)
library(sf)
tmap_mode('view')

#Leer el archivo metadata_estaciones_agrometAPI.csv (paso 1)
info_data <- read.csv('R/metadata_estaciones_agrometAPI.csv', sep = ";")

#Eliminar columnas x y unnamed del dataframe (paso 2)
info_data <- info_data%>% select (-one_of ('X', 'Unnamed..0'))

#convertir variables str a numericas (latitud y longitud) (paso 3)
info_data$latitud <- as.numeric(gsub(",", ".", info_data$latitud))
info_data$longitud <- as.numeric(gsub(",", ".", info_data$longitud))

class(info_data)
str(info_data)
print(info_data)

#Leer el archivo data_estaciones_agrometAPI.rds
data <- readRDS('R/data_estaciones_agrometAPI.rds')
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

#De los set de datos original debe filtrar las estaciones asignadas
estaciones_asignada <- l[["Alumno_5"]]
datos_filtrados <- data %>% filter(station_id %in% estaciones_asignada)
datos_filtrados

#Cree un script que permita calcular el promedio de las variables temp_promedio_aire, temp_minima y temp_maxima; 
#para todas las estaciones asignadas y todo el año 2021. Excluya del cálculo los valores no disponibles (NA).
promedio_temp_promedio_aire <- mean(datos_2021$temp_promedio_aire, na.rm = TRUE)
promedio_temp_minima <- mean(datos_2021$temp_minima, na.rm = TRUE)
promedio_temp_maxima <- mean(datos_2021$temp_maxima, na.rm = TRUE)
