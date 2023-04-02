library(shiny)
library(magick)
library(tidyverse)

v_taxa <- c("Quercus", "Cupressaceae", "Ambrosia", "Morus", "Pinaceae", "Ulmus early", "Ulmus late", "Fraxinus", "Betula", "Poaceae early", "Poaceae late", "Acer", "Populus")

shinyApp(
  ui = fluidPage(
    fluidRow(
      column(
        3,
        selectInput("taxa", "Taxa:", choices = v_taxa)
      ),
      column(
        9,
        selectInput("plot", "Plot:", choices = c(
          "ts_siteyear",
          "ts_site",
          "corr"
        ))
      ),
    ),
    plotOutput("plot")
  ),
  server = function(input, output) {
    img_file <- reactive(paste0("./data/results/", input$taxa, "/", input$plot, ".jpg"))
    output$plot <- renderImage(
      {
        tmp_file <- img_file() %>%
          image_read() %>%
          image_scale("50%") %>%
          image_write(tempfile(fileext = "jpg"), format = "jpg")
        list(src = tmp_file, contentType = "image/jpeg")
      },
      deleteFile = T
    )
  }
)