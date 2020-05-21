library(tidyverse)
library(plotly)

source("fxs.R")

# dados de despesas em saude
despesa<-read.csv2("data/despesa.csv")

# dados demograficos
demog<-read.csv2("data/demog.csv")
demog<-demog %>% pivot_longer(4:21, "g.etario", "n")
demog<-demog %>% mutate(value = ifelse(genero == "masculino", -1*value, value))

camas<-read.csv2("data/3-Camas_Hospitais.csv")
consultas<-read.csv2("data/2-consultas_internas.csv")
meds<-read.csv2("data/4-medicamentos.csv")


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {


#### separador despesas ######################
  
  valx<-reactive({ifelse(input$valor == "Despesa_Total","DespTotal",
               ifelse(input$valor == "Regime_Obrigatorio", "RegObrigat",
                      ifelse(input$valor == "Regime_Voluntario", "RegVolunt",
                             "DespFamilia")))
  })
  
  b1<-reactive({input$ano[1]})
  b2<-reactive({input$ano[2]})
  
  despesaT<-reactive({despesa %>% filter(ano >= b1() & ano <= b2())})

# distribuição anual 
  output$distPlot <- renderPlot({
    # point plot com despesas por ano
    gdt<-plotD(dados = despesaT(), yy = valx(), b1 = b1(), b2 = b2()) +
      ggtitle("Evolução anual das despesas de saúde")

    if(input$tipo == "area"){gdt + geom_area(fill = "#E1B378") + theme_bw()}
    else if(input$tipo == "barras"){gdt + geom_bar(stat="identity") + theme_bw()}
    else{gdt + geom_line(stat="identity", colour = "red") + theme_bw()}
  })

# percentagem do tipo de despesas
  output$percPlot <- renderPlot({
    dd<-despesaT() %>% select(ano, RegObrigat, RegVolunt, DespFamilia) %>% pivot_longer(cols = 2:4, names_to = "tipoDespesa")
    gpd<-ggplot(dd, aes(ano, value, fill = tipoDespesa)) + 
      geom_bar(stat="identity", position = "fill") +
      #scale_y_continuous(labels = percent_format()) +
      scale_x_continuous(breaks = b1():b2()) +
      labs(fill = "Tipo de despesa", y = "valor percentual (%)") +
      scale_fill_brewer(palette="Blues") + 
      theme_bw()
    gpd
  })
  
# Correlação entre as Despesas e o gasto em Hospitais  
  output$corrPlot <- renderPlot({
    gcr1<-ggplot(despesaT(),aes(x=HospPublicos, y=DespTotal)) + 
      geom_point() + geom_smooth() +
      labs(y = input$valor) +
      ggtitle("Correlação entre as despesas e o valor gasto em Hospitais públicos") +
      theme_bw()
    
    gcr2<-ggplot(despesaT(),aes(x=HospPublicos, y=RegObrigat)) + 
      geom_point() + geom_smooth() +
      labs(y = input$valor) +
      ggtitle("Correlação entre as despesas e o valor gasto em Hospitais públicos") +
      theme_bw()
    
    gcr3<-ggplot(despesaT(),aes(x=HospPublicos, y=RegVolunt)) + 
      geom_point() + geom_smooth() +
      labs(y = input$valor) +
      ggtitle("Correlação entre as despesas e o valor gasto em Hospitais públicos") +
      theme_bw()
    
    gcr4<-ggplot(despesaT(),aes(x=HospPublicos, y=DespFamilia)) + 
      geom_point() + geom_smooth() +
      labs(y = input$valor) +
      ggtitle("Correlação entre as despesas e o valor gasto em Hospitais públicos") +
      theme_bw()
    
    if(input$valor == "Despesa_Total"){gcr1}
    else if(input$valor =="Regime_Obrigatorio"){gcr2}
    else if(input$valor =="Regime_Voluntario"){gcr3}
    else if(input$valor =="Despesa_Familiar"){gcr4}
    
  })
  
#### separador Hospitais #################
# distribuição anual do nº de camas Hospitalares
  output$camasPlot <- renderPlot({
    ggplot(camas %>% filter(ano >= input$anoH[1] & ano <= input$anoH[2]), aes(x = ano)) +
      geom_line(aes(y = camas_HG, color = "camas Hospitais Gerais")) + 
      geom_point(aes(y = camas_HG, color = "camas Hospitais Gerais")) +
      geom_line(aes(y = camas_HE, color = "camas Hospitais Especializados")) + 
      geom_point(aes(y = camas_HE, color = "camas Hospitais Especializados")) +
      geom_line(aes(y = camas_HT, color = "camas totais")) + 
      geom_point(aes(y = camas_HT, color = "camas totais")) + 
      scale_colour_manual(values = c("#327345", "#ff4646", "#37c8ae")) +
      scale_x_continuous(breaks = seq(from = input$anoH[1], to = input$anoH[2], by = 1)) +
      scale_y_continuous(breaks = seq(from = 0, to = 30000, by = 5000)) +
      labs(y = "nº de camas",
           x = "Ano",
           colour = "",
           title = "Camas Hospitalares") + 
      theme_bw() + theme(legend.position = c(0.65, 0.3))
  })
  
  camax<-reactive({ifelse(input$cama == "camas Hospitais Gerais","camas_HG",
                         ifelse(input$cama == "camas Hospitais Especializados", "camas_HE",
                                "camas_HT"))
  })
  
  # join dados despesas e camas
  despesaH<-reactive({
    inner_join(despesa, camas) %>% filter(ano >= input$anoH[1] & ano <= input$anoH[2])
  })
  
# correlação entre despesas hospitalares e nº de camas
  output$corrCamas <- renderPlot({ 
    plotCor(dados = despesaH(), xx = "HospPublicos", yy = camax()) +
      labs(y = input$cama) + 
      theme_bw()
  })

# distribuição das consultas hospitalares
  output$consultasPlot <- renderPlot({
    ggplot(consultas %>% filter(ano >= input$anoH[1] & ano <= input$anoH[2]), aes(x = ano)) +
      geom_line(aes(y = cns_hospitais, color = "consultas hospitalares")) + 
      geom_point(aes(y = cns_hospitais, color = "consultas hospitalares")) +
      geom_line(aes(y = int_hospitais, color = "internamentos hospitalares")) + 
      geom_point(aes(y = int_hospitais, color = "internamentos hospitalares")) +
      geom_line(aes(y = urg_hospitais, color = "urgencias hospitalares")) + 
      geom_point(aes(y = urg_hospitais, color = "urgencias hospitalares")) + 
      scale_colour_manual(values = c("#327345", "#ff4646", "#37c8ae")) +
      scale_x_continuous(breaks = seq(from = input$anoH[1], to = input$anoH[2], by = 1)) +
      scale_y_continuous(breaks = seq(from = 0, to = 13000, by = 1000)) +
      labs(y = "nº de atendimentos",
           x = "Ano",
           colour = "",
           title = "Atendimentos Hospitalares") + 
      theme_bw() + theme(legend.position = c(0.65, 0.3))
  })
  
  
# legenda para yy
  consultax<-reactive({ifelse(input$consulta == "cns_hospitais","Consultas Hospitalares",
                          ifelse(input$consulta == "int_hospitais", "Internamentos Hospitalares",
                                 "Urgências Hospitalares"))
  })
  
  # join dados despesas e consultas
  despesaC<-reactive({
    inner_join(despesa, consultas) %>% filter(ano >= input$anoH[1] & ano <= input$anoH[2])
  })
  
  # correlação entre despesas hospitalares e nº de camas
  output$corrConsulta <- renderPlot({ 
    plotCor(dados = despesaC(), xx = "HospPublicos", yy = input$consulta) +
    labs(y = consultax()) +
      theme_bw()
  })
  
#### Separador medicamentos ####################
  
  
  # distribuição dos encargos com medicamentos
  output$medsPlot <- renderPlot({
    ggplot(meds %>% filter(ano >= input$anoM[1] & ano <= input$anoM[2]), aes(x = ano)) +
      geom_line(aes(y = sns, color = "SNS")) + 
      geom_point(aes(y = sns, color = "SNS")) +
      geom_line(aes(y = utente, color = "Utentes")) + 
      geom_point(aes(y = utente, color = "Utentes")) +
      geom_line(aes(y = total, color = "Total")) + 
      geom_point(aes(y = total, color = "Total")) + 
      scale_colour_manual(values = c("#327345", "#ff4646", "#37c8ae")) +
      scale_x_continuous(breaks = seq(from = input$anoM[1], to = input$anoM[2], by = 1)) +
      scale_y_continuous(breaks = seq(from = 0, to = 2500, by = 100)) +
      labs(y = "(milhoes €)",
           x = "Ano",
           colour = "",
           title = "Encargos com medicamentos") + 
      theme_bw() #+ theme(legend.position = c(0.65, 0.3))
  })
  
  
  # # legenda para yy
  valorMx<-reactive({ifelse(input$valorM == "DespTotal","Despesa Total",
                              ifelse(input$valorM == "RegObrigat", "Regime Obrigatorio",
                                     ifelse(input$valorM == "RegVolunt", "Regime Voluntario",
                                            ifelse(input$valorM == "DespFamilia", "Despesa Familiar",
                                     "Despesa de Hospitais Publicos"))))
  })
  
  # join dados despesas e consultas
  despesaM<-reactive({
    inner_join(despesa, meds) %>% filter(ano >= input$anoM[1] & ano <= input$anoM[2])
  })
  
  # correlação entre despesas hospitalares e nº de camas
  output$corrMeds <- renderPlot({ 
    plotCor(dados = despesaM(), xx = "total", yy = input$valorM) +
      labs(y = valorMx()) +
      labs(x = "Encargos totais com medicamentos (milhões €)") +
      theme_bw()
  })
  
#### separador demograficos  ############################
  anoP<-reactive({input$anoP})

  output$demoPlot <- renderPlot({
    dt<-demog %>% filter(ano == anoP())

    gdp<-ggplot(dt) +
      geom_bar(aes(x = g.etario, y = value, fill = genero), 
               subset(dt,dt$genero=="feminino"), stat = "identity") +
      geom_bar(aes(x = g.etario, y = value, fill = genero), 
               subset(dt,dt$genero=="masculino"), stat = "identity") +
      scale_y_continuous(breaks = seq(-400000, 400000, 100000),
                         labels = paste0(as.character(c(4:0, 1:4)), "E5")) +
      coord_flip() +
      scale_fill_brewer(palette = "Set1") +
      theme_bw()
    gdp

  })

  output$seniorPlot <- renderPlot({
    ggplot(inner_join(pop_65,mort_65) %>% filter(plus.65 == 1), aes(x=ano)) + 
      # geom_area(aes(y=n.pop), fill = "#abcdef") + 
      geom_line(aes(y=n.pop, color = "populacao > 65 anos")) + 
      geom_point(aes(y=n.pop, color = "populacao > 65 anos")) + 
      # geom_area(aes(y=n.mort), fill = "#ffd700") + 
      geom_line(aes(y=n.mort*22, color = "mortes > 65 anos")) + 
      geom_point(aes(y=n.mort*22, color = "mortes > 65 anos")) + 
      scale_y_continuous(sec.axis = sec_axis(~./22, name = "Mortes")) +
      scale_colour_manual(values = c("#ff4646", "#37c8ae")) +
      labs(y = "População",
           x = "Ano",
           colour = "",
           title = "População sénior ao longo dos anos") + 
      theme_bw() + theme(legend.position = c(0.65, 0.2))

    })
}
)


