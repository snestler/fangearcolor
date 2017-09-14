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

# Set the away team color and possible home colors
awayColor <- "Red"       #Red for now; may vary later
possColors <- c("Blue","Gold","Gray","Green","White")

# Define UI for application that draws an x-y scatter plot
ui <- fluidPage(
   
   # Application title
   titlePanel("Stadium Fan Gear Color Mix"),
   
   # Sidebar with a slider input for percentage of away fans
   sidebarLayout(
      sidebarPanel(
        
        sliderInput("awayPct",
                    label = "Away Team Color Percentage",
                    min = 0.0,
                    max = 1.0,
                    value = 0.25,
                    step = 0.05),
        
        selectInput("homeColors",
                    label = "Select Home Team Color(s) to Use",
                    unique(as.character(possColors)),
                    multiple = TRUE)
      ),
      
      # Show a plot of the rectangular seating area
      mainPanel(
         plotOutput("xyPlot")
      )
   )
)

# Define server logic required to draw the plot
server <- function(input, output) {
   
   output$xyPlot <- renderPlot({
    # generate plot based on awaypct and colors from ui.R
     print(input$homeColors)
     numHome <- length(input$homeColors)
     print(numHome)
     homepct <- (1-as.numeric(input$awayPct))/numHome
     probs <- c(rep(homepct,numHome),as.numeric(input$awayPct))
     print(probs)
     GearColors <- sample(c(input$homeColors,awayColor),
                     1656,
                     prob=probs,
                     replace=TRUE)
     dat2 <- data.frame(GearColors, seats = rep(1:46,36),
                        rows = rep(1:36,each=46))
      
      # draw the plot
      ggplot(dat2, aes(x=seats,y=rows,color=GearColors)) +
        geom_point(shape=15,size=3) +              #Use filled squares
        scale_color_manual(values=sort(c(input$homeColors,awayColor)))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

