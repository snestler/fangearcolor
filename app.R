#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(colourpicker)

# Define UI for application that draws an x-y scatter plot
ui <- fluidPage(
   
   # Application title
   titlePanel("Stadium Fan Gear Color Mix"),
   
   # Sidebar with a slider input for percentage of away fans
   sidebarLayout(
      sidebarPanel(

        colourInput("homeColor", "Select Home Team Color to Use", "#0C2340"),
        
        colourInput("awayColor", "Select Away Team Color", "red"),
        
        sliderInput("awayPct",
                    label = "Away Team Color Percentage",
                    min = 0.0,
                    max = 0.99,
                    value = 0.25,
                    step = 0.01)
        
      ),
      
      # Show a plot of the rectangular seating area
      mainPanel(
         plotOutput("xyPlot")
      )
   )
)

# Define server logic required to draw the plot
server <- function(input, output) {
  
  cols <- reactive({
    lapply(seq_along(dat), function(i) {
      colourInput(paste("col", i, sep="_"), "Choose colour:", "black")        
    })
  })
  
  selectedData <- reactive({
    numHome <- length(input$homeColor)
    homePct <- (1-as.numeric(input$awayPct))/numHome
    probs <- c(rep(homePct,numHome),as.numeric(input$awayPct))
    print(probs)
    GearColors <- sample(c(input$homeColor,input$awayColor),
                         1656,
                         prob=probs,
                         replace=TRUE)
    dat2 <- data.frame(GearColors, seats = rep(1:46,36),
                       rows = rep(1:36,each=46))
    
    return(dat2)
  })
  
  output$xyPlot = renderPlot({
    ggplot(selectedData(), aes(x=seats,y=rows,color=GearColors)) +
      geom_point(shape=15,size=3) +              #Use filled squares
      scale_color_manual(values=sort(c(input$homeColor,input$awayColor))) +
      theme_minimal()
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

