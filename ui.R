library(shiny)
library(shinythemes)
library(plotly)
library(shinydashboard)

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(#theme = "bootstrap.min.css",
    theme = shinytheme("spacelab"), 
    headerPanel(title ="",
                windowTitle = "HEADS"),
    titlePanel("HEADS | LAB"),
    
    navbarPage("Application Maravilha",
               tabPanel("Despesas", icon = icon("money"),
                      # Application title
                      #titlePanel("opções para os gráficos"),
                      # Sidebar with options 
                      sidebarPanel(
                        a("opções para os gráficos"),
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
                        submitButton("aplicar alterações", icon = icon("refresh"))
                        ),
                      # Show a plot of despesas
                      mainPanel(
                        titlePanel("Representações gráficas"),
                        plotOutput("distPlot"),
                        plotOutput("corrPlot"),
                        textOutput("test") ## linha para apagar
                        )
                      ),
             ##### novo tab para resultados GHD e outros
             tabPanel("GHD", icon = icon("list-alt")
                      ),
             tabPanel("mais um", icon = icon("accessible-icon")
                      ),
             tabPanel("outro", icon = icon("address-card")
                      )
             )
    )
)



