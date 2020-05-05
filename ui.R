library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = "bootstrap.min.css",
  
  headerPanel("New Application"),
  
  # Application title
  titlePanel("sidebar Title"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      titlePanel("Panel title"),
      
      plotOutput("distPlot")
    )
  )
)