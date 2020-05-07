library(tidyverse)
library(rlang)

#despesa<-read.csv2("data/despesa.csv")

plotD<-function(dados = despesa, yy = "DespTotal", b1 = b1, b2 = b2){
  yy<-sym(yy)
  ggplot(dados, aes(x=ano, y=!!yy)) +
    geom_point() + scale_x_continuous(breaks = b1:b2) 
}

#plotD(yy="RegObrigat") + geom_bar(stat="identity")
