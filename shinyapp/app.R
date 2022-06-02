library(shiny)
library(magick)
library(tidyverse)

taxa_list <- c("Quercus", "Cupressaceae", "Ambrosia", "Morus", "Pinaceae", "Ulmus early", "Ulmus late", "Fraxinus", "Betula", "Poaceae early", "Poaceae late", "Acer", "Populus")

shinyApp(
  ui = fluidPage(
    fluidRow(
      column(3,
             selectInput("taxa", "Taxa:", choices = taxa_list)),
      column(9,
             selectInput("plot", "Plot:", choices = c("flower frequency compared with other data",
                                                      "phenology across site and year",
                                                      "flower frequency and pollen count correlation"))),
    ),
    
    plotOutput("plot")
  ),
  
  server = function(input, output) {
    img_file<-reactive(paste0("./result_figs/",input$taxa,"/",input$plot, ".jpg"))
    output$plot = renderImage({
      tmpfile <- img_file() %>% image_read() %>% image_scale("50%") %>% 
        image_write(tempfile(fileext='jpg'), format = 'jpg')
      list(src = tmpfile, contentType = "image/jpeg")
    },
    deleteFile = T)
  }#,
  
  # options = list(width = 40, height=40)
)