#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Team C Shiny Group Assignment"),
    
    # Show a plot of the generated distribution
    mainPanel(
      img(src='ie3.jpeg', align = "right"),

        #Select POWER PLANT
        selectInput("plants", label = "Select the plant to show on the Map",
                    choices = station_info$stid,
                    selected = c("ACME",station_info$stid),
                    width='55%',
                    multiple = FALSE),
        
        #Select the DATE-FRAME
        dateRangeInput("daterange", label = "Select the date range to plot the plants power generation", 
                       start = "1994-01-01", 
                       end = "2007-12-31", 
                       min = "1994-01-01",
                       max = "2007-12-31",
                       autoclose = TRUE
                     
                    ),
 
        #Data table output
        fluidRow(
          plotOutput("powergenerated"), leafletOutput("mymap"),
            column(12, DT::dataTableOutput('table')))
        
    )
)
)