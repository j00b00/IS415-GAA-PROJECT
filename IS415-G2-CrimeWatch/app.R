pacman::p_load(shiny, sf, tidyverse, 
               tmap, bslib)

hunan <- st_read(dsn = "data/hunan/geospatial", layer = "Hunan")

data <- read_csv("data/hunan/aspatial/Hunan_2012.csv")
hunan_data <- left_join(hunan, data, 
                        by = c("County" = "COUNTY"))

ui <- fluidPage(
  titlePanel("Choropleth Mapoing"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "Variable",
                  label = "Mapping variable",
                  choices = list("Gross Domestic Product" = "GDP",
                                 "Gross Domestic Product Per Captia" = "GDPPC",
                                 "Gross Industry Output" = "GIO",
                                 "Output Value of Agriculture" = "OVA",
                                 "Output Value of Service" = "OVS"),
                  selected = "GDPPC"),
      sliderInput(inputId = "classes",
                  label = "Number of classes",
                  min = 5, 
                  max = 10, 
                  value = c(6))
    ),
    
    mainPanel(
      plotOutput("mapPlot",
                 width = "100%",
                 height = 400)
    )
  )
)

server <- function(input, output){
  output$mapPlot <- renderPlot({
    tmap_options(check.and.fix = TRUE) +
      tm_shape(hunan_data)+
      tm_fill(input$Variable,
              n = input$classes,
              style = "quantile",
              palette = blues9) +
      tm_borders(lwd = 0.1, alpha = 1)
  })
}

shinyApp(ui = ui, server = server)
