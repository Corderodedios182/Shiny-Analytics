library(lubridate)
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(tidyverse)
library(shinythemes)
library(gapminder)
library(reactlog)
library(plotly)
library(gapminder)
library(leaflet)
library(wordcloud)
library(wordcloud2)
library(tm)

my_css <- "
#download_data {
  /* Change the background color of the download button
     to orange. */
  background: blue;

  /* Change the text size to 20 pixels. */
  font-size: 15px;
}

#table {
  /* Change the text color of the table to red. */
  color: purple;
}
"

ui <- fluidPage(
    
    titlePanel(h1(strong(em("Monitoreo Eventos Analytics")))),
    theme = shinytheme("simplex"),
    #shinythemes::themeSelector(),
    tags$style(my_css),
    
    #3.Update Layout (UI)
    sidebarLayout(
        sidebarPanel(
            
            fileInput("file", "Download File"),
            
            pickerInput(
                inputId = "event",
                label = "Select Event", 
                choices = unique(data_new$eventCategory),
                multiple = TRUE ,
                selected = unique(data_new$eventCategory)[3]),
            br(),
            pickerInput(
              inputId = "action",
              label = "Select Action", 
              choices = unique(data$eventAction),
              multiple = TRUE ,
              selected = unique(data$eventAction)[10]),
            br(),
            pickerInput(
              inputId = "label",
              label = "Select de Label", 
              choices = unique(data$eventLabel),
              multiple = TRUE ,
              selected = unique(data$eventLabel)[10]),
            br(),
            dateRangeInput('date_range', 'Select Date', "2010-01-01", "2019-12-01"),
            br(),
            downloadButton(outputId = "download_data", label = "Descargar información")
            ),
        
        mainPanel(
            em(h2("Datos y Graficas interactivas")),
            tabsetPanel(
              tabPanel("Monitoreo y Alertas"),
              tabPanel("Datos Originales")))
        )
)
