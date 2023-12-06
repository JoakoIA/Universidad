library(agrometR)
library(tidyverse)

data_era5 <- read_rds("R/datos_temp_media_ERA5Land_estaciones_agromet.rds")

   <- read_rds("R/data_agromet_proyecto_final_2015-2022.rds")

#Cambiar los numeros de las estaciones por los codigos emas
emas <- estaciones_agromet %>% 
  pull(ema)

names(data_era5)[2:418] <- emas

#Seleccionar las columnas de interes
cod_emas <- estaciones_agromet %>%
  filter(region == "Biobío") %>%
  pull(ema)

cod_emas <- as.character(cod_emas)

data_era5_Biobio <- data_era5 |> 
  dplyr::mutate(date = lubridate::ymd(date)) |> 
  dplyr::filter(dplyr::between(date, ymd("2015-01-01"), ymd("2022-12-31")))  |> 
  dplyr::select(date, all_of(cod_emas))

readr::write_rds(data_era5_Biobio,'R/data_era5_arica_2015-2022.rds')

data_agro_sel <- data_agromet |> 
  dplyr::select(station_id:temp_promedio_aire) |> 
  dplyr::filter(dplyr::between(fecha_hora,ymd("2015-01-01"),ymd("2022-12-01"))) |> 
  dplyr::filter(station_id %in% as.numeric(cod_emas)) 

data_agro_sel |> 
  dplyr::group_by(station_id,
                  mes = lubridate::floor_date(fecha_hora,'month')) |> 
  dplyr::summarize(NAs = sum(is.na(temp_promedio_aire)),.groups = 'drop') |> 
  mutate(station_id = as.factor(station_id),
         mes_plot = format(mes, "%y-%b")) |> 
  ggplot(aes(mes_plot,station_id,fill=NAs)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
