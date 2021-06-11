setwd("3_GitHub/Shiny-Analytics/")

eval(parse('get_data.R', encoding = 'UTF-8'))

data <- procces_info("Datos/Dataset Eventos 2020 - Sheet1.csv")
