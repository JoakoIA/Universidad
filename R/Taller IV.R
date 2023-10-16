library(readr)
library(tidyverse)

data <- read_rds("GitHub/Universidad/R/data_agvAPI.rds")
info <- read_rds("GitHub/Universidad/R/metadata_estaciones_agvAPI.rds")

#------------------------------------------Ejercicios con datos agromet------------------------------------------#

data_agromet <- read_rds("GitHub/Universidad/R/Anterior/data_estaciones_agrometAPI.rds")
info_agromet <- read.csv2("GitHub/Universidad/R/Anterior/metadata_estaciones_agrometAPI.csv")

