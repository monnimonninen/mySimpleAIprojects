#monarch K-means app

monarkki_5var <- read.csv(file.path("monarch_5var.csv"))

library(readr)  # for read_csv
library(shiny)  

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

ui <- fluidPage(
      headerPanel('Monarkkiperhonen, k-means klusterointi'),
            sidebarPanel(
              
                  #valikko 1
                  selectInput(
                    'xcol', 
                    'X Variable', 
                    names(monarkki_5var)),
                  
                  #valikko 2
                  selectInput(
                    'ycol', 
                    'Y Variable', 
                    names(monarkki_5var),
                            
                    
                  #valittuina nimet githubdatastani??                           
                  selected = names(monarkki_5var)[[2]]),    
                  
                  #valikko 3
                  numericInput(
                    'clusters', 
                    'Cluster count', 
                    3, 
                    min = 1, 
                    max = 9)
                  ),
  
      mainPanel(
                  plotOutput('plot1')
  )
)

server <- function(input, output) {

#make reactive data
                  selectedData <- reactive({
                    monarkki_5var[, c(input$xcol, input$ycol)]
                    })
                
                  clusters <- reactive({
                    kmeans(selectedData(), input$clusters)
                    })

 #renderplot 
                  output$plot1 <- renderPlot({
                    par(mar = c(5.1, 4.1, 0, 1))
                    plot(selectedData(),
                         col = clusters()$cluster, pch = 20, cex = 3)
                    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
                  })

}

shinyApp(ui = ui, server = server)
