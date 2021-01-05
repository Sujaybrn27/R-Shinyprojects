library(shiny)
library(leaflet)
library(RColorBrewer)
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body{width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range","Magnitudes", min(toronto$magnitude), max(toronto$magnitude), value = c(range(toronto$magnitude)), step = .5),
  selectInput("clrs", "color scheme", choices = rownames(subset(brewer.pal.info, category %in% c("seq","div")))),
  checkboxInput("legend","Enable Legend", TRUE)
  ))
server <- function(input, output, session){
  filtereddata <- reactive({
    toronto[toronto$magnitude>=input$range[1] & toronto$magnitude<=input$range[2],]
  })
  colorpal <- reactive({
    colorNumeric(input$clrs, toronto$magnitude)
  })
  output$map <- renderLeaflet({
    leaflet(toronto) %>% addTiles() %>% fitBounds(min(toronto$Long), min(toronto$Lat), max(toronto$Long), min(toronto$Lat))
  })
  observe({
    pal <- colorpal()
    leafletProxy("map", data = filtereddata()) %>%
      clearShapes() %>%
      addCircles(radius = 10, weight = 5, color = "red", fillColor = ~pal(toronto$magnitude),
                 fillOpacity = 1, popup = ~paste(toronto$Name,toronto$Address, sep = ",")
                 )
  })
  observe({
    proxy <- leafletProxy("map", data = toronto)
    proxy %>% clearControls()
    if(input$legend){
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright", pal = pal, values = toronto$magnitude)
    }
  })
}
shinyApp(ui = ui, server = server)