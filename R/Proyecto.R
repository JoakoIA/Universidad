library(agrometR)
library(tidyverse)
library(dbscan)
library(sf)

#Cargar datos
data_era5 <- read_rds("R/datos_temp_media_ERA5Land_estaciones_agromet.rds")
data_agromet <- read_rds("R/data_agromet_proyecto_final_2015-2022.rds")

#Cambiar los numeros de las estaciones por los codigos emas
emas <- estaciones_agromet %>% 
  pull(ema)

names(data_era5)[2:418] <- emas
#Cambiar los nombres de las columnas
data_era5 %>% 
  glimpse()

#filtrar por region
cod_emas <- estaciones_agromet %>%
  filter(region == "Araucanía") %>%
  pull(ema)

cod_emas <- as.character(cod_emas)

cod_emas %>%
  length()


data_agromet %>%
  glimpse()

#Seleccionar las columnas de interes, filtrar por intervalo de tiempo y filtrar para las estaciones seleccionadas
data_agromet_sel <- data_agromet %>%
  select(station_id:temp_promedio_aire) %>%
  filter(between(fecha_hora, ymd("2015-01-01"), ymd("2022-12-31"))) %>%
  filter(station_id %in% as.numeric(cod_emas))

## Analisis de NA
#Maximo de NA = 6
#Visualizar cantidad de dias con NAs por mes
data_agromet_sel %>%
  group_by(station_id, mes = floor_date(fecha_hora, 'month')) %>%
  summarize(NAs = sum(is.na(temp_promedio_aire)), .groups = 'drop') %>%
  mutate(station_id = as.factor(station_id), mes_plot = format(mes, "%y-%b")) %>%
  ggplot(aes(mes_plot, station_id, fill = NAs)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
  ggsave("R/NA_mes.png", width = 10, height = 10, units = "cm", dpi = 300)


#Filtrar NAs > 20%
data_agro_fillNAs <- data_agromet_sel %>%
  complete(station_id, fecha_hora, fill = list(temp_promedio_aire = NA)) %>%
  group_by(station_id, mes = floor_date(fecha_hora, 'month')) %>%
  summarize(temp = mean(temp_promedio_aire, na.rm = TRUE),
  propNAs = sum(is.na(temp_promedio_aire))/n(), .groups = 'drop') %>%
  filter(propNAs < 0.2) %>%
  select(-4)

# Visualizar cantidad de dias con NAs por mes
data_agro_fillNAs %>%
  ggplot(aes(mes, as.factor(station_id), fill = temp)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
  ggsave("R/NA_mes_fillNA.png", width = 10, height = 10, units = "cm", dpi = 300)

#Filtrar por cantidad de observaciones > 80%
data_agro_fillNAs %>%
  ungroup() %>%
  complete(station_id, mes, fill = list(temp = NA)) %>%
  group_by(station_id) %>%
  summarize(prop_NAs = sum(is.na(temp))/n()) %>%
  ggplot(aes(as.factor(station_id), prop_NAs)) +
  geom_col() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
  ggsave("R/NA_estacion.png", width = 10, height = 10, units = "cm", dpi = 300)

emas_sel <- data_agro_fillNAs %>%
  ungroup() %>%
  complete(station_id, mes, fill = list(temp = NA)) %>%
  group_by(station_id) %>%
  summarize(prop_NAs = sum(is.na(temp))/n()) %>%
  filter(prop_NAs < 0.2)

data_agro_fillNAs %>%
  filter(station_id %in% emas_sel)

#Hacer un agrupamiento para poder resumir el analisis
library(sf)

emas_Araucania_sel <- data_agro_fillNAs %>%
  ungroup() %>%
  distinct(station_id) %>%
  pull(station_id)

emas_Araucania <- estaciones_agromet %>%
  filter(ema %in% emas_Araucania_sel)

clus <- emas_Araucania %>%
  st_as_sf(coords = c("longitud", "latitud"), crs = 4326) %>%
  st_transform(32718) %>%
  st_coordinates() %>%
  as.matrix() %>%
  dbscan(20000, minPts = 5)

clus <- clus$cluster

emas_Araucania1 <- emas_Araucania %>%
  mutate(cluster = clus) %>%
  select(ema, cluster)

emas_Araucania1 %>%
  left_join(emas_Araucania) %>%
  ggplot(aes(longitud, latitud, color = as.factor(cluster))) +
  geom_point(size = 5) +
  theme_bw()
  ggsave("R/cluster.png", width = 10, height = 10, units = "cm", dpi = 300)

#unir los datos de temperatura con los datos de las estaciones
data_agro2 <- data_agro_fillNAs %>%
  left_join(emas_Araucania1, by = c('station_id' = "ema"))

#boxplot de temperatura por cluster
data_agro2 %>%
  ggplot(aes(as.factor(cluster), temp)) +
  geom_jitter(alpha = 0.7, color = 'lightblue', size = 0.3) +
  geom_violin(outlier.shape = NA, fill = NA, colour = 'darkgreen') +
  geom_boxplot(color = 'black', fill = NA) +
  theme_bw()
  ggsave("R/boxplot_cluster.png", width = 10, height = 10, units = "cm", dpi = 300)

#boxplot de temperatura por estacion
data_agro2 %>%
  ggplot(aes(as.factor(station_id), temp)) +
  geom_boxplot() +
  facet_grid(.-cluster, scales = 'free_x') +
  theme_bw()
  ggsave("R/boxplot_estacion.png", width = 10, height = 10, units = "cm", dpi = 300)
