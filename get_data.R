library(tidyverse)
library(ggplot2)
library(lubridate)
library(car)
library(plotly)
library(stringr)

#- Funciones de Apoyo

get_n <- function(x, n_init = 1, n_end = 4) {
  #Extraccion de n digitos en una cadena de texto
  x %>%
    as.character() %>%
    substr(n_init, n_end)
}

procces_info <- function(data){
  #Procesa Insumos para algoritmo  
  data <- read_csv(data)
  
  colnames(data) <- colnames(data) %>%
    str_replace_all("ga.","")
  
  data <- data %>%
    mutate(year = get_n(data$date,1,4),
           mes = get_n(data$date,5,6),
           dia = get_n(data$date,7,8),
           fecha = ymd(as.Date(paste0(year, "/",mes,"/", dia))))
}
