library(agrometR)
library(tidyverse)

data_era5 <- read_rds("R/datos_temp_media_ERA5Land_estaciones_agromet.rds")

data_agromet <- read_rds("R/data_agromet_proyecto_final_2015-2022.rds")

#Cambiar los numeros de las estaciones por los codigos emas
emas <- estaciones_agromet %>% 
  pull(ema)

names(data_era5)[2:418] <- emas

data_era5 %>% 
  glimpse()

#Modificar la fecha de tipo caracter a tipo fecha
data_era5 %>% 
  mutate(date = ymd(date)) %>% 
  filter(between(date,ymd("2015-01-01"),ymd("2022-12-31")))
  select(all_of(cod_emas))

cod_emas <- estaciones_agromet %>%
  filter(region == "Biobío") %>%0
  pull(ema)

cod_emas <- cod_emas[1:30]

data_era5_a <- data_era5 %>% 
  mutate(date = ymd(date)) %>% 
  filter(between(date,ymd("2015-01-01"),ymd("2022-12-31"))) %>%
  select(all_of(cod_emas))
#Seleccionar valores unicos de la columna region
estaciones_agromet %>% 
  select(region) %>% 
  unique()

#Datos agromet
data_agromet %>% 
  glimpse()

#Seleccionar las columnas de interes
data_agromet %>%
  select(station_id:temp_promedio_aire) %>%
  filter(between(fecha_hora, ymd("2015-01-01"), ymd("2022-12-31"))) %>%
  filter(station_id %in% cod_emas)
