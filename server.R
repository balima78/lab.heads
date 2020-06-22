library(tidyverse)
library(plotly)

source("fxs.R")

# dados de despesas em saude
despesa<-read.csv2("data/despesa.csv")
despesa <- despesa %>% mutate(DespTotal = round(DespTotal/1000000,2),
                              RegObrigat = round(RegObrigat/1000000, 2),
                              RegVolunt = round(RegVolunt/1000000, 2),
                              DespFamilia = round(DespFamilia/1000000, 2), 
                              HospPublicos = round(HospPublicos/1000000, 2)
                                )

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
  
  valx<-reactive({ifelse(input$valor == "Despesa Total","DespTotal",
               ifelse(input$valor == "Regime Obrigatorio", "RegObrigat",
                      ifelse(input$valor == "Regime Voluntario", "RegVolunt",
                             "DespFamilia")))
  })
  
  b1<-reactive({input$ano[1]})
  b2<-reactive({input$ano[2]})
  
  despesaT<-reactive({despesa %>% filter(ano >= b1() & ano <= b2())})

# distribuição anual 
  output$distPlot <- renderPlot({
    # point plot com despesas por ano
    gdt<-plotD(dados = despesaT(), yy = valx(), b1 = b1(), b2 = b2()) +
      ggtitle("Evolução anual das despesas de saúde (em mil milhões de €)")

    if(input$tipo == "area"){gdt + geom_area(fill = "#E1B378") + theme_bw()}
    else if(input$tipo == "barras"){gdt + geom_bar(stat="identity") + theme_bw()}
    else{gdt + geom_line(stat="identity", colour = "red") + theme_bw()}
  })
  
# descritivo para a distribuição anual
  output$explOrca <- renderText({
    if(input$valor == "Despesa Total"){"Na primeira metade deste século, verificou-se um aumento gradual e contínuo na despesa total anual em Saúde. A partir de 2011 e até 2014 houve um declínio deste valor anual com uma recuperação gradual até 2018 em que se superou o valor registado em 2010."}
    else if(input$valor =="Regime Obrigatorio"){"Os gastos em Saúde originários do Regime Obrigatótio, tiveram um crescimento entre os anos 2000 e 2010, um declinio até 2014 e uma recuperação constante até 2018."}
    else if(input$valor =="Regime Voluntario"){"Os gastos em Saúde suportados por Regimes voluntários têm aumentado praticamente todos os anos ainda que de forma ligeira. Os maiores aumentos registaram-se no ínicio deste século."}
    else if(input$valor =="Despesa Familiar"){"As despesas familiares em Saúde têm também aumentado anualmente, com excepção da variação entre 2012 e 2013 em que houve uma ligeira diminuição. Nos últimos anos este aumento tem sido mais acentuado."}
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
  
# descrição das percentagens
  output$explPerc <- renderText({
    "Embora os valores anuais do regime obrigatório representem claramente a maior percentagem das despesas em saúde, podemos verificar um aumento anual da percentagem das despesas familiares e que parece ir compensando os valores do regime obrigatório."
  })
  
# Correlação entre as Despesas e o gasto em Hospitais  
  output$corrPlot <- renderPlot({
    gcr1<-ggplot(despesaT(),aes(x=HospPublicos, y=DespTotal)) + 
      geom_point() + geom_smooth() +
      labs(y = paste(input$valor,"(mil milhões de €)"),
           x = "Gastos Hospitalares (mil milhões de €)") +
      ggtitle("Correlação entre as despesas e o valor gasto em Hospitais públicos") +
      theme_bw()
    
    gcr2<-ggplot(despesaT(),aes(x=HospPublicos, y=RegObrigat)) + 
      geom_point() + geom_smooth() +
      labs(y = paste(input$valor,"(mil milhões de €)"),
           x = "Gastos Hospitalares (mil milhões de €)") +
      ggtitle("Correlação entre as despesas e o valor gasto em Hospitais públicos") +
      theme_bw()
    
    gcr3<-ggplot(despesaT(),aes(x=HospPublicos, y=RegVolunt)) + 
      geom_point() + geom_smooth() +
      labs(y = paste(input$valor,"(mil milhões de €)"),
           x = "Gastos Hospitalares (mil milhões de €)") +
      ggtitle("Correlação entre as despesas e o valor gasto em Hospitais públicos") +
      theme_bw()
    
    gcr4<-ggplot(despesaT(),aes(x=HospPublicos, y=DespFamilia)) + 
      geom_point() + geom_smooth() +
      labs(y = paste(input$valor,"(mil milhões de €)"),
           x = "Gastos Hospitalares (mil milhões de €)") +
      ggtitle("Correlação entre as despesas e o valor gasto em Hospitais públicos") +
      theme_bw()
    
    if(input$valor == "Despesa Total"){gcr1}
    else if(input$valor =="Regime Obrigatorio"){gcr2}
    else if(input$valor =="Regime Voluntario"){gcr3}
    else if(input$valor =="Despesa Familiar"){gcr4}
    
  })
  
  # descritivo para a correlação com os gastos hospitalares
  output$explRel <- renderText({
    if(input$valor == "Despesa Total"){"Verifica-se uma correlação lienar entre a despesa total anual em Saúde e os gastos anuais dos hospitais públicos."}
    else if(input$valor =="Regime Obrigatorio"){"Podemos verificar uma correlação linear entre o valor anual do regime obrigatório e os gastos hospitalares."}
    else if(input$valor =="Regime Voluntario"){"Não existe uma correlação clara entre os valores anuais afectos ao regime voluntário e os gastos hospitalares anuais."}
    else if(input$valor =="Despesa Familiar"){"Não existe uma correlação clara entre os valores anuais afectos às despesas fas famílias e os gastos hospitalares anuais."}
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
  
  # descritivo para a correlação entre consultas e gastos hospitalares
  output$explCamas <- renderText({
    if(input$cama == "camas Hospitais Gerais"){"Não podemos afirmar que exista uma correlação linear entre o número de camas anuais  dos  hospitais gerais e a despesa global anual dos hospitais publicos."}
    else if(input$cama =="camas Hospitais Especializados"){"Não existe uma evidência clara de correlação linear entre o número anual de camas em hospitais especializados e os gastos dos hospitais."}
    else if(input$cama =="camas Hospitalares totais"){"Ao contrário do que seria de esperar, podemos verificar que há uma tendência inversamente proporcional entre o número de camas hospitalares disponíveis e os gatos anuais dos hospitais públicos."}
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
    labs(y = consultax(),
         x = "Gastos dos Hospitais Públicos (mil milhões de €)") +
      theme_bw()
  })
  
  # descritivo para a correlação entre consultas e gastos hospitalares
  output$explCons <- renderText({
    if(input$consulta == "Consultas Hospitalares"){"Podemos verificar que existe uma correlaõa entre os gastos hospitalares e o número de con sultas anuais."}
    else if(input$consulta =="Internamentos Hospitalares"){"Não existe uma correlação entre o número anual de internamentos hospitalares e os gastos dos hospitais."}
    else if(input$consulta =="Urgências Hospitalares"){"Não existe uma correlação clara entre os valores anuais afectos ao regime voluntário e os gastos hospitalares anuais."}
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
      geom_vline(xintercept = 2010.5, linetype="dotted", 
                 color = "red", size=1.5) + 
      geom_vline(xintercept = 2014.5, linetype="dotted", 
                 color = "green", size=1.5) +
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
                         labels = c(rev(seq(0,400,100)),seq(100,400,100))) +
      xlab("grupo etário") +
      ylab("milhares de pessoas") +
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
  
##################### GDHs ##############################  
  output$gdh10PlotN <- renderPlotly({
    p1<-ggplot(gdh10N,
              aes(x=ano, y=value, group=gdh)) +
      geom_line(aes(color=factor(gdh))) +
      geom_point(aes(color=factor(gdh))) +
      scale_x_continuous(breaks = 2000:2016) +
      labs(y = "nº de GDHs",
           x = "Ano",
           colour = "top 10 GDHs",
           title = "Evolução do número anual de GDHs") +
      theme_bw() + theme()

    ggplotly(p1)
  })

  output$gdh10PlotV <- renderPlotly({
    p2<-ggplot(gdh10V,
              aes(x=ano, y=round(value / 1000000,2), group=gdh)) +
      geom_line(aes(color=factor(gdh))) +
      geom_point(aes(color=factor(gdh))) +
      scale_x_continuous(breaks = 2000:2016) +
      labs(y = "valor (milhoes de €)",
           x = "Ano",
           colour = "top 10 GDHs",
           title = "Evolução do valor por GDHs ao longo dos anos") +
      theme_bw() + theme()

    ggplotly(p2)
  })
  
  output$designaGDH<-renderTable({
    gdhDesigna
  })

}
)


