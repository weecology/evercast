library(shiny)

ui <- pageWithSidebar(
  headerPanel("Forecasts"),
  sidebarPanel(
    sliderInput("origin", "Forecast Origin",
                min = 2001, max = 2016,  value = 2001, sep = "")
  ),
  mainPanel(
    # Use imageOutput to place the image on the page
    imageOutput("preImage")
  )
)



server <- function(input, output, session) {

  output$preImage <- renderImage({
    filename <- normalizePath(file.path('./forecasts',
                              paste0("nb_origin_", input$origin, ".tiff")))

message(filename) 

    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Observed as a function of predicted for ", input$origin))

  }, deleteFile = FALSE)
}


shinyApp(ui, server)