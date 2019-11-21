#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table) #We need to add the library
library(leaflet)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    compute_plot <- reactive({
     
      plot(x = (strptime(solar_data[["Date"]], format= "%Y%m%d")[(strptime(solar_data[["Date"]], format= "%Y%m%d")>strptime(input$daterange[1], format= "%Y-%m-%d")) & 
                                                                   (strptime(solar_data[["Date"]], format= "%Y%m%d")<strptime(input$daterange[2], format= "%Y-%m-%d")) ]), 
           y = (solar_data[[input$plants]])[(strptime(solar_data[["Date"]], format= "%Y%m%d")>strptime(input$daterange[1], format= "%Y-%m-%d")) & 
                                        (strptime(solar_data[["Date"]], format= "%Y%m%d")<strptime(input$daterange[2], format= "%Y-%m-%d"))], 
           type = "l", 
           xlab = "Input DateRange", ylab = "Power Generated",
           col = "orange")
    })
    

    output$powergenerated <- renderPlot({
        compute_plot();
    })
    
    compute_data <- reactive({
      if (solar_data[[input$plants]] == solar_data[[input$plants]]){
        dat <- as.data.table(as.array(summary((solar_data[[input$plants]])[(strptime(solar_data[["Date"]], format= "%Y%m%d")>strptime(input$daterange[1], format= "%Y-%m-%d")) & 
                                                         (strptime(solar_data[["Date"]], format= "%Y%m%d")<strptime(input$daterange[2], format= "%Y-%m-%d"))])))
      } 
      return(dat)
    })
    
    # Print table
    output$table = DT::renderDataTable(
      compute_data(), rownames=FALSE)
    

    
    output$mymap <- renderLeaflet({
        leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng = station_info[station_info$stid == input$plants,"elon"], lat = station_info[station_info$stid == input$plants,"nlat"], popup = as.character(input$plants))
    })
    

    
})
