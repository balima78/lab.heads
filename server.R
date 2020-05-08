library(tidyverse)

source("fxs.R")

# dados de despesas em saude
despesa<-read.csv2("data/despesa.csv")

# dados demograficos
demog<-read.csv2("data/demog.csv")
demog<-demog %>% pivot_longer(4:21, "g.etario", "n")
demog<-demog %>% mutate(value = ifelse(genero == "masculino", -1*value, value))


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  valx<-reactive({ifelse(input$valor == "Despesa_Total","DespTotal",
               ifelse(input$valor == "Regime_Obrigatorio", "RegObrigat",
                      ifelse(input$valor == "Regime_Voluntario", "RegVolunt",
                             "DespFamilia")))
  })
  
  b1<-reactive({input$ano[1]})
  b2<-reactive({input$ano[2]})
  
  despesaT<-reactive({despesa %>% filter(ano >= b1() & ano <= b2())})

  output$distPlot <- renderPlot({
    # point plot com despesas por ano
    gdt<-plotD(dados = despesaT(), yy = valx(), b1 = b1(), b2 = b2()) +
      ggtitle("Evolução anual das despesas de saúde")

    if(input$tipo == "area"){gdt + geom_area(fill = "#E1B378") + theme_bw()}
    else if(input$tipo == "barras"){gdt + geom_bar(stat="identity") + theme_bw()}
    else{gdt + geom_line(stat="identity", colour = "red") + theme_bw()}
  })
  
  output$corrPlot <- renderPlot({
    gcr1<-ggplot(despesaT(),aes(x=HospPublicos, y=DespTotal)) + 
      geom_point() + geom_smooth() +
      ggtitle("Correlação entre as despesas e o valor gasto em Hospitais públicos") +
      theme_bw()
    
    gcr2<-ggplot(despesaT(),aes(x=HospPublicos, y=RegObrigat)) + 
      geom_point() + geom_smooth() +
      ggtitle("Correlação entre as despesas e o valor gasto em Hospitais públicos") +
      theme_bw()
    
    gcr3<-ggplot(despesaT(),aes(x=HospPublicos, y=RegVolunt)) + 
      geom_point() + geom_smooth() +
      ggtitle("Correlação entre as despesas e o valor gasto em Hospitais públicos") +
      theme_bw()
    
    gcr4<-ggplot(despesaT(),aes(x=HospPublicos, y=DespFamilia)) + 
      geom_point() + geom_smooth() +
      ggtitle("Correlação entre as despesas e o valor gasto em Hospitais públicos") +
      theme_bw()
    
    if(input$valor == "Despesa_Total"){gcr1}
    else if(input$valor =="Regime_Obrigatorio"){gcr2}
    else if(input$valor =="Regime_Voluntario"){gcr3}
    else if(input$valor =="Despesa_Familiar"){gcr4}
    
  })
  

  output$demoPlot <- renderPlot({
    
    ggplot(demog %>% filter(ano == 2018), 
           aes(x = g.etario, y = value, fill = genero)) + 
      geom_bar(subset = .(genero == "feminino"), stat = "identity") + 
      geom_bar(subset = .(genero == "masculino"), stat = "identity") + 
      scale_y_continuous(breaks = seq(-400000, 400000, 100000), 
                         labels = paste0(as.character(c(4:0, 1:4)), "E5")) + 
      coord_flip() + 
      scale_fill_brewer(palette = "Set1") + 
      theme_bw()
    
  })
}
)