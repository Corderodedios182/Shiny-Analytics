setwd("3_GitHub/Shiny-Analytics/")

eval(parse('get_data.R', encoding = 'UTF-8'))

data <- procces_info("Datos/Dataset Eventos 2020 - Sheet1.csv")

#Etiquetado de Datos
traffic_lights  <- function(data){
  
  a <- list()
  #Para cada eventCategory colocamos el semaforo
  for(i in seq(1,length(unique(data$eventCategory)))){
      tmp <- filter(data,eventCategory == unique(data$eventCategory)[i])
      tmp <- if(dim(tmp)[1] < 3){
              tmp <- rbind(tmp, tmp[2,])
             }else{
              tmp}
      
      #Logica: para Etiquetar con un Rango de 3 dias
      if(dim(tmp)[1] == length(rep(seq(1, round(dim(tmp)[1]/3), 1), each = 3))){
        tmp["rango_3_dias"] <- rep(seq(1, floor(dim(tmp)[1]/3), 1), each = 3)
      }else if(dim(tmp)[1] > length(rep(seq(1, round(dim(tmp)[1]/3), 1), each = 3))){
        tmp["rango_3_dias"] <- c(rep(seq(1, floor(dim(tmp)[1]/3), 1), each = 3),max(rep(seq(1, floor(dim(tmp)[1]/3), 1), each = 3)) + 1)
      }else{
        tmp["rango_3_dias"] <- c(rep(seq(1, floor(dim(tmp)[1]/3), 1), each = 3),max(rep(seq(1, floor(dim(tmp)[1]/3), 1), each = 3)) + 1,1)
      }
      #Agrupacion y variables Eventos
      df <- tmp %>% 
        group_by(eventCategory, rango_3_dias) %>%
        summarise(
          max_fecha = max(fecha),
          suma = sum(totalEvents),
          promedio = mean(totalEvents),
          sd = sd(totalEvents),
          mediana = median(totalEvents),
          minimo = min(totalEvents),
          max = max(totalEvents))
      
      #Logica: para comparar contra los días anteriores
      df["dif_event_dia_ant"] <- c(0,diff(df$max))
      
      df["dia_ant"] <- if(df["dif_event_dia_ant"] < 0){
        df$max + abs(df$dif_event_dia_ant)
      }else{
        df$max - df$dif_event_dia_ant
      }
      
      diferencia <- function(x,y){
        if(x <= y){
          (x/y - 1) * 100
        }else{
          (y/x)*100
        }
      }
      
      df["diferencia"] <- round(diferencia(df$max,df$dia_ant))
      
      #Etiquetado con el Maximo el Rango por Evento
      df["etiquetado"] <- ""
      df[df["diferencia"] >= -34,"etiquetado"] <- "verde"
      df[(df["diferencia"] <= -35) & (df["diferencia"] >= -50),"etiquetado"] <- "amarillo"
      df[(df["diferencia"] < -50) & (df["diferencia"] >= -89),"etiquetado"] <- "naranja"
      df[df["diferencia"] <= -90,"etiquetado"] <- "rojo"
      
      df["diferencia"] <- df["diferencia"]/100 
      
      a[[i]] <- df
      } #Termina for
  
  data_new <- do.call(rbind, a)
  return(data_new)
}

data_new <- traffic_lights(data)
