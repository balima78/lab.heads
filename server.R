library(tidyverse)

despesa<-read.csv2("data/despesa.csv")


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  despesaT<-reactive({despesa %>% filter(ano >= input$ano[1] & ano <= input$ano[2])})
  
  valorx<-reactive({input$valor})
  
  output$test<-renderText({
    valx<-ifelse(valorx() == "Despesa_Total","DespTotal",
                 ifelse(valorx() == "Regime_Obrigatorio", "RegObrigat",
                        ifelse(valorx() == "Regime_Voluntario", "RegVolunt",
                               "DespFamilia"))
                 )
    })

  output$distPlot <- renderPlot({
    # line plot com despesa total por ano
    gdt<-ggplot(despesaT(), aes(x=ano, y=DespTotal)) +
      geom_point() + scale_x_continuous(breaks = input$ano[1]:input$ano[2]) +
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
}
)