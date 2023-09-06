library(tidyverse)

info_data <- readCSV('data_estaciones_agrometAPI.rds')
class(info_data)

info_data <- read.csv("metadata_estaciones_agrometAPI.csv")