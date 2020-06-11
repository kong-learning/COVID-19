library(shiny)
library(leaflet)
library(htmltools)
library(DT)
library(jsonlite)
library(geojsonio)
library(dplyr) 
library(RColorBrewer)
library(scales)
library(lattice)
library(googleCharts)
library(ggplot2)
library(ggthemes) 
library(rsconnect)
library(rlang)
library(ggrepel)

# Latitude and longitude
df <- read.csv(textConnection(
  "Name,Lat,Long
  柬埔寨,12.5657,104.9910
  文莱,4.5353,114.7277
  印尼,0.7893,113.9213
  老挝,19.8563,102.4955
  马来西亚,4.2105,101.9758
  菲律宾,12.8797,121.7740
  新加坡,1.3521,103.8198
  泰国,15.8700,100.9925
  越南,14.0583,108.2772
  缅甸,21.9162,95.9560"
))

# Latitude and longitude
pre <- read.csv(textConnection(
  "Name,result
  柬埔寨,10+
  文莱,20
  印尼,30
  老挝,40
  马来西亚,50
  菲律宾,60
  新加坡,70
  泰国,80
  越南,90
  缅甸,20"
))

vis_data <-merge(df,pre,by="Name")


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
      leaflet(vis_data) %>% addTiles() %>% addCircleMarkers() %>% addMarkers(~Long, ~Lat, label = ~htmlEscape(result))
    })
  

  output$data <- renderTable({
    head((dftable()[, c("Country", input$typeofvariable), drop = FALSE]) ,10)}, rownames = TRUE)
  
  #
  output$table <- DT::renderDataTable({
    DT::datatable(analyticsData)
    })
}

shinyApp(ui, server)
























































# dfmap<-reactive ({
#   analytics2<-filter(dataAnalytics,Date==target_year())
#   CountryData_<-left_join(Df1,analytics2,by="Country")
#   CountryData_%>%select(input$typeofvariable)
# })

# Create the map
# output$map <- renderLeaflet({
#   leaflet(geojson) %>% addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
#                                 attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>')%>%
#     addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.5,
#                 label = paste(CountryData$Country, ":", dfmap()[,1]),
#                 color = pal(rescale(dfmap()[,1],na.rm=TRUE))
#     )%>%
#     setView(lng = 0, lat = 40, zoom = 2) %>%
#     addLegend("bottomleft",pal = pal, 
#               
#               values =c(0:1), opacity = 0.7)
# })



# ui <- bootstrapPage(
# tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
# leafletOutput("mymap", width = "100%", height = "100%"),
# absolutePanel(top = 10, right = 10,
#               sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
#                           value = range(quakes$mag), step = 0.1
#               ),
#               selectInput("colors", "Color Scheme",
#                           rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
#               ),
#               checkboxInput("legend", "Show legend", TRUE)
#               )




#   
#   # This reactive expression represents the palette function,
#   # which changes as the user makes selections in UI.
#   colorpal <- reactive({
#     colorNumeric(input$colors, quakes$mag)
#   })
#   
#   # Reactive expression for the data subsetted to what the user selected
#   filteredData <- reactive({
#     quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
#   })
#   
#   # Incremental changes to the map (in this case, replacing the
#   # circles when a new color is chosen) should be performed in
#   # an observer. Each independent set of things that can change
#   # should be managed in its own observer.
#   observe({
#     pal <- colorpal()
#     
#     leafletProxy("mymap", data = filteredData()) %>%
#       clearShapes() %>%
#       addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
#                  fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
#       )
#   })
#   output$mymap <- renderLeaflet({
#     leaflet(vis_data) %>% addTiles() %>% addCircleMarkers() %>% addMarkers(~Long, ~Lat, label = ~htmlEscape(result))
#   })
#   
#   
#   # Use a separate observer to recreate the legend as needed.
#   observe({
#     proxy <- leafletProxy("mymap", data = quakes)
#     
#     # Remove any existing legend, and only if the legend is
#     # enabled, create a new one.
#     proxy %>% clearControls()
#     if (input$legend) {
#       pal <- colorpal()
#       proxy %>% addLegend(position = "bottomright",
#                           pal = pal, values = ~mag
#       )
#     }
#   })
# }