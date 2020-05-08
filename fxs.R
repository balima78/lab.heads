library(tidyverse)
library(rlang)

# plot despesas por ano
plotD<-function(dados, yy = "DespTotal", b1, b2){
  yy<-sym(yy)
  ggplot(dados, aes(x=ano, y=!!yy)) +
    geom_point() + scale_x_continuous(breaks = b1:b2) 
}



#### Piramides demográficas
gdemo <- function(dados, yy){
  ggplot(dados %>% filter(ano == yy), 
         aes(x = g.etario, y = value, fill = genero)) + 
  geom_bar(subset = .(genero == "feminino"), stat = "identity") + 
  geom_bar(subset = .(genero == "masculino"), stat = "identity") + 
  scale_y_continuous(breaks = seq(-400000, 400000, 100000), 
                     labels = paste0(as.character(c(4:0, 1:4)), "E5")) + 
  coord_flip() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()
}
