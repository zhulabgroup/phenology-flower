library(shiny)
library(magick)
library(tidyverse)

v_taxa <- c("Acer", "Alnus", "Betula", "Carya", "Celtis", "Fraxinus", "Juglans", "Liquidambar", "Morus", "Platanus", "Populus", "Quercus", "Salix", "Ulmus early", "Ulmus late")
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
    img_file <- reactive(paste0("./result_figs/", input$taxa, "/", input$plot, ".jpg"))
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
