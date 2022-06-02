library(shiny)
shinyApp(
    
    ui = fluidPage(
        selectInput("taxa", "Taxa:", choices = taxa_list),
        selectInput("plot", "Plot:", choices = c("flower frequency compared with other data",
                                                 "phenology across site and year",
                                                 "flower frequency and pollen count correlation")),
        plotOutput("plot")
    ),
    
    server = function(input, output) {
        img_file<-reactive(paste0("/raid/users/ysong67/GitHub/phenology/RS4flower/output/data/",input$taxa,"/",input$plot, ".jpg"))
        output$plot = renderPlot({
            image_ggplot(image_read(img_file()))
        })
    },
    
    options = list(width = 1600, height=1600)
)