library(shiny)
library(leaflet)
library(htmltools)
library(DT)
library(jsonlite)
library(dplyr) 
library(RColorBrewer)
library(scales)
library(lattice)
library(ggplot2)
library(rsconnect)
library(rlang)
library(ggrepel)

vis_data <- read.csv("pre.csv")
analyticsData<-read.csv("csv_for_inquire.csv")
va <- names(analyticsData)
vars <-va[-1:-2]
Date<-analyticsData$Date

# Define UI for application that draws a histogram
ui <- navbarPage("Covid-19", id="nav",
                 tabPanel("Interactive Map",
                          div(class="outer",
                              tags$head
                              (
                                # Include our custom CSS
                                includeCSS("styles.css")
                              ),
                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              leafletOutput("map", width="100%", height="100%"),
                              # Shiny versions prior to 0.11 should use class = "modal" instead.
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = FALSE, top = 55, left = "auto", right = 10, bottom = "auto",
                                            width = 350, height = "100%",
                                            h2("Covid-19 Data Search"),
                                            selectInput("typeofDate", "Select Dates", Date),
                                            selectInput("typeofvariable", "Select variables", vars),
                                            tableOutput("data")
                              )
                          )
                 ),
                 # tab 'DataSearch'
                 tabPanel("DataTable",DTOutput(outputId = "table"))
)



server <- function(input, output, session) {
  #Get query date
  target_date = reactive({
    input$typeofDate
  })
  
  #Get query type
  target_quo = reactive ({
    parse_quosure(input$typeofvariable)
  })
  
  #Query fixed-type variables by date and then sort
  dftable<-reactive({
    analytics=filter(analyticsData,Date== target_date())
    arrange(analytics,desc(!!target_quo()))
  })
  
  output$map <- renderLeaflet({
    leaflet(vis_data) %>% addTiles() %>% addCircleMarkers() %>% addMarkers(~Long, ~Lat, label = ~htmlEscape(cfr))
  })
  
  
  output$data <- renderTable({
    head((dftable()[, c("Country", input$typeofvariable), drop = FALSE]) ,10)}, rownames = TRUE)
  
  #
  output$table <- DT::renderDataTable({
    DT::datatable(analyticsData)
  })
}

shinyApp(ui, server)