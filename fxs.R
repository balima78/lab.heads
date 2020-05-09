library(tidyverse)
library(rlang)

# plot despesas por ano
plotD<-function(dados, yy = "DespTotal", b1, b2){
  yy<-sym(yy)
  ggplot(dados, aes(x=ano, y=!!yy)) +
    geom_point() + scale_x_continuous(breaks = b1:b2) 
}

