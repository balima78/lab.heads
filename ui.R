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
                        plotOutput("corrPlot")
                        )
                      ),
             ##### novos tab para resultados GHD e outros
             tabPanel("Hospitais", icon = icon("hospital-alt"),
                      sidebarPanel(
                        a("Hospitais Publicos"),
                        wellPanel(
                          sliderInput("anoH", "Seleccione intervalo:",
                                    min = 2000, max = 2018, step = 1, sep = "",
                                    value = c(2000,2018))
                          ),
                        wellPanel(
                          selectInput("cama", "Tipo de camas hospitalares",
                                      c("camas Hospitais Gerais","camas Hospitais Especializados",
                                        "camas Hospitalares totais"))
                          ),
                        wellPanel(
                          selectInput("consulta", "Tipo de atendimentos hospitalares",
                                          choices = c("Consultas Hospitalares" = "cns_hospitais",
                                                      "Internamentos Hospitalares" = "int_hospitais",
                                                      "Urgências Hospitalares" = "urg_hospitais")
                                      )
                            ),
                        submitButton("aplicar alterações", icon = icon("refresh"))
                        ),
                      mainPanel(
                        plotOutput("camasPlot"),
                        plotOutput("corrCamas"),
                        plotOutput("consultasPlot"),
                        plotOutput("corrConsulta")
                        )
                      ),
             tabPanel("Medicamentos", icon = icon("pills"),
                      sidebarPanel(
                        a("Medicamentos"),
                        wellPanel(
                          sliderInput("anoM", "Seleccione intervalo:",
                                      min = 2000, max = 2018, step = 1, sep = "",
                                      value = c(2000,2018))
                        ),
                        wellPanel(
                          selectInput("valorM", "Tipo de despesas",
                                      c("Despesa Total" = "DespTotal",
                                        "Regime Obrigatorio" = "RegObrigat",
                                        "Regime Voluntario" = "RegVolunt",
                                        "Despesa Familiar" = "DespFamilia",
                                        "Despesa de Hospitais Publicos" = "HospPublicos"))
                        ),
                        submitButton("aplicar alterações", icon = icon("refresh"))
                      ),
                      mainPanel(
                        plotOutput("medsPlot"),
                        plotOutput("corrMeds")
                      )
             ),
             tabPanel("GHD", icon = icon("list-alt")
                      ),
             tabPanel("População", icon = icon("users"),
                      sidebarPanel(
                        a("opções para os gráficos"),
                        wellPanel(sliderInput("anoP", "Selecione ano",
                                              # animate = animationOptions(interval = 1600, loop = TRUE),
                                              min = 2000, max = 2018,
                                              value = 2009, step = 1, sep = ""
                                              )
                        ),
                        submitButton("aplicar alterações", icon = icon("refresh"))
                      ),
                      mainPanel(
                        titlePanel("Piramide demográfica de Portugal"),
                        plotOutput("demoPlot"),
                        plotOutput("seniorPlot")
                      )
                      ),
             tabPanel("outro", icon = icon("address-card")
                      )
             )
    )
)



