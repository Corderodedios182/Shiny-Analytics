data <- procces_info("Datos/Dataset Eventos 2020 - Sheet1.csv")

eval(parse('2_modules/get_data.R', encoding = 'UTF-8'))

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
          promedio = round(mean(totalEvents),2),
          sd = round(sd(totalEvents),2),
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

#- Analisis Exploratorio
print_percentage <- scales::label_comma(accuracy = 0.1, scale = 100, suffix = '%')

group_by(data_new, etiquetado) %>% count(conteo = n())
group_by(data_new, eventCategory, etiquetado) %>% count(conteo = n())

data_event <- filter(data_new, eventCategory == unique(data_new$eventCategory)[3])

summary_event <- group_by(data_event, etiquetado) %>% summarise(conteo = n()) %>% mutate(porcentaje = print_percentage(conteo/sum(conteo)))

#Graficas de Control
fig <- plot_ly(
  data = data_event,
  x = ~max_fecha,
  hoverinfo = 'text'
) %>%
  add_lines(y = ~diferencia,
            name = 'Eventos diferencia porcentual',
            marker = list(color = "#0052ce"),
            line = list(color = '#0052ce', width = 2),
            fill = '#0052ce',
            hovertext = ~max_fecha
  ) %>%
  layout(
    title = paste0('Monitoreo de Eventos cada 3 días:  ', unique(data_event$eventCategory)),
    xaxis = list(title = "Revisar días que sobre pasan la Línea Crítica", tickangle = 0),
    yaxis = list(
      title = 'Diferencia Respecto a los días anteriores',
      range = c(min(data_event$diferencia)/.35, max(data_event$diferencia)/.95),
      tickformat = '%,2.f'
    ),
    legend = list(x = .40, y = -.25, orientation = 'h'),
    margin = list(r = -3),
    hovermode = 'compare',
    shapes = lines
  ) %>%
  add_lines(y=-.90,
            name = 'Línea Crítica',
            line = list(color = '#ce0015', width = 1),
            text = "-90%"
  )%>%
  add_lines(y=-.35,
            name = 'Alerta I',
            line = list(color = '#cfc625', width = 1),
            text = "-35%"
  )%>%
  add_lines(y=-.50,
            name = 'Alerta II',
            line = list(color = '#ce6e00', width = 1),
            text = "-50%")
fig
