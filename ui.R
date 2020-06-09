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
    
    navbarPage("Costs 4 Health",
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
                                      c("Despesa Total","Regime Obrigatorio",
                                        "Regime Voluntario","Despesa Familiar"))
                          )
                        # ,
                        # submitButton("aplicar alterações", icon = icon("refresh"))
                        ),
                      # Show a plot of despesas
                      mainPanel(
                        titlePanel("Orçamentos anuais"),
                        plotOutput("distPlot"),
                        a(href="https://www.ine.pt/xportal/xmain?xpid=INE&xpgid=ine_destaques&DESTAQUESdest_boui=354229895&DESTAQUESmodo=2&xlang=pt", 
                          "fonte: Instituto Nacional de Estatística"),
                        br(),
                        textOutput("explOrca"), 
                        br(noWS = 5),
                        plotOutput("percPlot"),
                        textOutput("explPerc"),
                        br(noWS = 5),
                        plotOutput("corrPlot"),
                        br(),
                        textOutput("explRel")
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
                            )
                        # ,
                        # submitButton("aplicar alterações", icon = icon("refresh"))
                        ),
                      mainPanel(
                        plotOutput("camasPlot"),
                        hr("Ao longo do presente século temos assistido a uma diminuição anual do número de camas hospitalares, tanto em hospitais gerais como especializados."),
                        plotOutput("corrCamas"),
                        a(href="https://www.pordata.pt/Portugal/SNS+camas+nos+estabelecimentos+de+sa%c3%bade+++Continente-938", 
                          "fonte: PORDATA"),
                        textOutput("explCamas"),
                        plotOutput("consultasPlot"),
                        hr("Desde o ano 2000, O número de consultas hospitalares tem aumentado anualmente, enquanto que os internamentos hospitalares têm tido uma ligeira diminuição."),
                        plotOutput("corrConsulta"),
                        a(href="https://www.pordata.pt/Portugal/SNS+consultas++internamentos+e+urg%c3%aancias+++Continente-159", 
                          "fonte: PORDATA"),
                        textOutput("explCons")
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
                        )
                        # ,
                        # submitButton("aplicar alterações", icon = icon("refresh"))
                      ),
                      mainPanel(
                        plotOutput("medsPlot"),
                        hr("Na área do medicamento, os resultados das políticas de contenção que Portugal teve de adotar para cumprir o Memorando de Entendimento (Plano da Tróica), estão bem evidentes no gráfico “Encargos com Medicamentos”. Senão vejamos, desde início do século 21 até ao ano 2010 há um registo crescente da despesa nesta área. É em 2010 que há uma desaceleração acentuada na despesa até ao ano 2013, altura em que muito lentamente se inverte esta tendência. Houve um aumento de 245,9 milhões de euros (11,1%) de despesa entre 2015 e 2018, 173 milhões de euros no setor hospitalar e 72,8 milhões de euros no setor ambulatório. Este aumento de despesa permitiu um importante reforço do acesso à inovação terapêutica mediante a aprovação de 51 novos medicamentos em 2016, 60 em 2017 e 40 em 2018. "),
                        plotOutput("corrMeds"),
                        a(href="https://www.pordata.pt/Portugal/SNS+encargos+com+medicamentos+++Continente-327", 
                          "fonte: PORDATA")
                      )
             ),
             tabPanel("GDH", icon = icon("list-alt"),
                      mainPanel(
                        plotlyOutput("gdh10PlotN"),
                        hr("Os 10 GDH mais prevalentes no ano 2000 registaram uma evolução estável ao longo do tempo. Os GDH 371, 39 e 381 registaram uma tendência decrescente em relação provável com novas orientações técnicas."),
                        plotlyOutput("gdh10PlotV"),
                        hr("Os 10 GDH com custo mais elevado mantiveram-se globalmente estáveis, mas destaca-se a tendência crescente dos GDH 818, 209, 483 e 585."),
                        br(),
                        hr("Legenda dos códigos de GDH"),
                        tableOutput("designaGDH"),
                        a(href="", 
                          "fonte: Base de Dados Nacional de GDH")
                      )
                      ),
             tabPanel("População", icon = icon("users"),
                      sidebarPanel(
                        a("opções para os gráficos"),
                        wellPanel(sliderInput("anoP", "Selecione ano",
                                              # animate = animationOptions(interval = 1600, loop = TRUE),
                                              min = 2000, max = 2018,
                                              value = 2009, step = 1, sep = ""
                                              )
                        )
                        # ,
                        # submitButton("aplicar alterações", icon = icon("refresh"))
                      ),
                      mainPanel(
                        titlePanel("Piramide demográfica de Portugal"),
                        plotOutput("demoPlot"),
                        a(href="", 
                          "fonte: INE"),
                        plotOutput("seniorPlot"),
                        a(href="https://evm.min-saude.pt/#shiny-tab-q_idade", 
                          "fonte: DGS")
                      )
                      ),
             tabPanel("Material e Métodos", icon = icon("cogs"),
                      h5("Com o objectivo de perceber como é gasto o dinheiro em Saúde, propusemo-nos a recolher dados de acesso livre sobre gastos de saúde em Portugal."),
                      h5("Usamos como fontes os dados disponibilizados pelo Instiuto Nacional de Estatística, o site da PORDATA e o site da transparência do SNS."),
                      h5("Com recurso ao package Shiny do R, desenvolvemos esta aplicação. Aqui podemos correlacionar os gastos anuais de Saúde em Portugal com parâmetros como gastos hospitalares, gastos com medicamentos ou os GDHs.")
                      )
             )
    )
)



