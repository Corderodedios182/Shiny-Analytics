#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$graph_control <- renderPlotly({fig})
    
    output$table_origin <- renderDataTable(
        datatable(
            {
                data
            }, 
            rownames = FALSE, 
            options = list(
                scrollX = TRUE,
                scrollY = TRUE,
                pageLength = 100
            )
        )
    )
    
    output$summary <- renderDataTable(
        datatable(
            {
                summary_event
            }, 
            rownames = FALSE, 
            options = list(
                scrollX = TRUE,
                scrollY = TRUE,
                pageLength = 100
            )
        )
    )
    
    output$table <- renderDataTable(
        datatable(
            {
                data_event
            }, 
            rownames = FALSE, 
            options = list(
                scrollX = TRUE,
                scrollY = TRUE,
                pageLength = 100
            )
        )
    )
    })
