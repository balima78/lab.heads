library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  
  fluidPage(theme = "bootstrap.min.css",
  
  headerPanel("Application Maravilha"),
  
  # Application title
  titlePanel("opções para os gráficos"),

  # Sidebar with a slider input for number of bins 
      sidebarPanel(
        
        wellPanel(
          sliderInput("ano", "Seleccione intervalo:",
                  min = 2000, max = 2018, step = 1, sep = "",
                  value = c(2000,2018)),
          radioButtons("tipo", "Seleccione tipo de gráfico:",
                   c("linhas", "area", "barras"),
                   inline = TRUE)
        ),
        wellPanel(
          selectInput("valor", "Tipo de despesas", 
                      c("Despesa_Total","Regime_Obrigatorio",
                        "Regime_Voluntario","Despesa_Familiar"))
        ),

      submitButton("aplicar alterações")
    ),
    
    # Show a plot of the generated distribution

    mainPanel(
      
      titlePanel("Representações gráficas"),
      
      plotOutput("distPlot"),
      
      plotOutput("corrPlot")
    )
  )
)

