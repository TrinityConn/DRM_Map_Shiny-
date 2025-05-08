#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(leaflet)

ui <- fluidPage(
  titlePanel("Species Density Map (DRM Sites 2024)"),
  fluidRow(
    column(
      width = 2,
      selectInput("species", "Select a Species:", choices = NULL),
      verbatimTextOutput("hoverCoords")
    ),
    column(
      width = 10,
      leafletOutput("speciesMap", height = "800px")
    )
  )
)

server <- function(input, output, session) {
  observe({
    species_choices <- unique(dens2$Species)
    updateSelectInput(session, "species", choices = species_choices)
  })
  
  species_data <- reactive({
    req(input$species)
    dens2 %>% filter(Species == input$species)
  })
  
  output$speciesMap <- renderLeaflet({
    data <- species_data()
    pal <- colorNumeric("viridis", domain = data$n)
    
    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude,
        radius = ~sqrt(n),
        color = ~pal(n),
        fillOpacity = 0.7,
        stroke = FALSE,
        label = ~paste0("Lat: ", round(Latitude, 4), 
                        " Lon: ", round(Longitude, 4), 
                        " Site: ", Site),
        popup = ~paste("Density:", round(n, 2), 
                       "<br>Subregion:", Subregion)
      ) %>%
      addTiles() %>%
      addDrawToolbar(
        drag = TRUE,
        editOptions = editToolbarOptions(
          selectedPathOptions = selectedPathOptions()
        )
      ) %>%
      addLegend("bottomright", pal = pal, values = ~n,
                title = paste("Density per DRM Site 2024 -", input$species),
                opacity = 1) %>%
      onRender("
        function(el, x) {
          var map = this;
          function updateCoords(e) {
            var lat = e.latlng.lat.toFixed(5);
            var lng = e.latlng.lng.toFixed(5);
            Shiny.setInputValue('hover_coords', {lat: lat, lng: lng}, {priority: 'event'});
          }
          map.on('mousemove', updateCoords);
        }
      ")
  })
  
  output$hoverCoords <- renderText({
    coords <- input$hover_coords
    if (is.null(coords)) return("Hover over the map to see coordinates.")
    paste0("Latitude: ", coords$lat, " | Longitude: ", coords$lng)
  })
}

shinyApp(ui, server)
