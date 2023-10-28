library(readr)
library(tidyverse)
info_data <- read_rds('R/metadata_estaciones_agrometAPI.rds')
data <- read_rds('R/data_estaciones_agrometAPI.rds')

set.seed(876)
ids <- data$station_id |> 
  unique() |> 
  sample(50)

data <- subset(data, station_id %in% ids)
info_data <- subset(info_data, ema %in% ids)


#(10pts) Haga una union de los dos set de datos por el código de la estación (utilice la función merge). 
#En el caso de info_data el código es ema y para data es station_id. Guarde la unión en un objeto con nombre data_unida.
data_unida <- merge(info_data, data, by.x = 'ema', by.y = 'station_id')
print(head(data_unida))

#(40pts) Cree dos variables adicionales que identifiquen a cada observación con respecto a la estación del año 
#(invierno, otoño, primavera, verano) y el mes; y agréguela al objeto data_unida.
data_unida <- data_unida |>
  mutate(
    estacion = case_when(
      fecha_hora %in% seq(as.POSIXct('2020-12-20 00:00:00'), as.POSIXct('2021-03-21 00:00:00'), by = 'hour') ~ 'Verano',
      fecha_hora %in% seq(as.POSIXct('2021-03-21 00:00:00'), as.POSIXct('2021-06-21 00:00:00'), by = 'hour') ~ 'Otoño',
      fecha_hora %in% seq(as.POSIXct('2021-06-21 00:00:00'), as.POSIXct('2021-09-21 00:00:00'), by = 'hour') ~ 'Invierno',
      fecha_hora %in% seq(as.POSIXct('2021-09-21 00:00:00'), as.POSIXct('2021-12-21 00:00:00'), by = 'hour') ~ 'Primavera',
      TRUE ~ NA_character_
    )) |>
  mutate(
    mes = month(fecha_hora) |> 
      factor(levels = c(1:12), labels = c('Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo', 'Junio', 
      'Julio', 'Agosto', 'Septiembre', 'Octubre', 'Noviembre', 'Diciembre')))

#Elige la primera fecha
max(data_unida$fecha_hora)

print(head(data_unida))

#Cree una nueva variable respecto a las macrozonas de Chile (macrozona), 
#de acuerdo a como están definidas en odes-unidades (Norte grande, Norte chico, Zona central, Zona sur, Zona austral).
data_unida <- data_unida |>
  mutate(macrozona = case_when(
    region %in% c('Valparaíso', 'Metropolitana', 'O\'Higgins', 'Maule') ~ 'Zona central',
    region %in% c('Biobío', 'Araucanía', 'Los Ríos', 'Los Lagos') ~ 'Zona sur',
    region %in% c('Aysén', 'Magallanes') ~ 'Zona austral',
    region %in% c('Antofagasta', 'Arica y Parinacota', 'Tarapacá') ~ 'Norte grande',
    region %in% c('Coquimbo') ~ 'Norte chico',
    TRUE ~ NA_character_
  )) |>
  mutate(macrozona = factor(macrozona, levels = c('Norte grande', 'Norte chico', 'Zona central', 'Zona sur', 'Zona austral'))) 

print(head(data_unida))
table(data_unida$macrozona)

#Seleccione las variables correspondientes a fecha_hora,mes,ema,institución,macrozona,estacion,region,temp_promedio_aire,
#precipitacion_horaria,humed_rel_promedio. Utilice la función subset. Guarde los datos enel objeto data_vis.
data_vis <- subset(data_unida, select = c(fecha_hora, mes, ema, institucion, macrozona, estacion, region, temp_promedio_aire, precipitacion_horaria, humed_rel_promedio))
print(head(data_vis))

#(10pts) Haga un grafico en ggplot que se muestre la variación de la temperatura promedio del aire en el tiempo, 
#utilice como geometria puntos (geom_point).

ggplot(data_vis, aes(x = fecha_hora, y = temp_promedio_aire)) +
  geom_point(size = 2, alpha = 0.3) +
  labs(x = 'Fecha', y = 'Temperatura promedio del aire', title = 'Variación de la temperatura promedio del aire en el tiempo')

#Modifique el gráfico anterior para que los puntos se coloreen de acuerdo a la institución a la que pertenece los datos.

ggplot(data_vis, aes(x = fecha_hora, y = temp_promedio_aire, color = institucion)) +
  geom_point(size = 2, alpha = 0.3) +
  labs(x = 'Fecha', y = 'Temperatura promedio del aire', title = 'Variación de la temperatura promedio del aire en el tiempo')

#(30pts) Cree paneles (facets) para separar los datos de acuerdo a la diferentes estaciones del año 
#(otoño, primavera, verano, invierno). Modifique de ser necesario para mejorar la visualización.

ggplot(data_vis, aes(x = fecha_hora, y = temp_promedio_aire, color = institucion)) +
  geom_point(size = 2, alpha = 0.3) +
  labs(x = 'Fecha', y = 'Temperatura promedio del aire', title = 'Variación de la temperatura promedio del aire en el tiempo') +
  facet_wrap(~estacion, nrow = 2)

#(20pts) ¿Qué problema de visualización presenta el gráfico anterior a su juicio?
#Respuesta: Los datos se encuentran muy dispersos, por lo que es difícil visualizar la tendencia de los datos.

#(20pts) Cambie la geometria de visualización por un tipo de gráfico de boxplot (geom_boxplot). 
#Cambie además la variación temporal de fecha_hora a mes.

ggplot(data_vis, aes(x = mes, y = temp_promedio_aire, color = institucion)) +
  geom_boxplot() +
  labs(x = 'Mes', y = 'Temperatura promedio del aire', title = 'Variación de la temperatura promedio del aire en el tiempo') +
  facet_wrap(~estacion, nrow = 2)


# Modifique el gráfico para que los paneles se muestren en grillas (geom_grid) por estacion y macrozona

ggplot(data_vis, aes(x = mes, y = temp_promedio_aire, color = institucion)) +
  geom_boxplot() +
  labs(x = 'Mes', y = 'Temperatura promedio del aire', title = 'Variación de la temperatura promedio del aire en el tiempo') +
  facet_grid(estacion ~ macrozona)

#(40pts) Cree un grafico con paneles de grilla entre estacion y macrozona; 
#en dónde se visualize la relación entre temperatura promedio del aire y la humedad relativa.

ggplot(data_vis, aes(x = temp_promedio_aire, y = humed_rel_promedio, color = institucion)) +
  geom_point(size = 2, alpha = 0.3) +
  labs(x = 'Temperatura promedio del aire', y = 'Humedad relativa promedio', 
  title = 'Relación entre temperatura promedio del aire y humedad relativa') +
  facet_grid(macrozona ~ estacion)

#(30pts) Haga un grafico en el que se pueda ver la variación temporal (mes), para las diferentes variables climáticas (temperatura, humedad relativa y precipitación), cada una en un panel diferente y por panel de macrozona.
library(tidyr)

data_vis_long <- data_vis %>%
  pivot_longer(cols = c(temp_promedio_aire, precipitacion_horaria, humed_rel_promedio),
               names_to = "variable", values_to = "value")

ggplot(data_vis_long, aes(x = mes, y = value, color = institucion)) +
  geom_line(size = 1) +
  labs(x = 'Mes', y = 'Valor', title = 'Variación temporal de variables climáticas por macrozona') +
  facet_grid(variable ~ macrozona, scales = 'free', switch = 'y') +
  scale_color_discrete(name = "Institución") +
  theme_minimal()


#Como ver las columnas de data_vis
names(data_vis)
