library(tidyverse)
library(rlang)

# plot despesas por ano
plotD<-function(dados, yy = "DespTotal", b1, b2){
  yy<-sym(yy)
  ggplot(dados, aes(x=ano, y=!!yy)) +
    geom_point() + scale_x_continuous(breaks = b1:b2) 
}

# plot correlações
plotCor<-function(dados, xx = "HospPublicos", yy = "camasHT"){
  xx<-sym(xx)
  yy<-sym(yy)
  ggplot(dados, aes(x=!!xx, y=!!yy)) +
    geom_point() + geom_smooth() +
    theme_bw()
  }


# preparar ficheiro de mortalidade por grupo etário
mort_et<-read.csv2("data/mortalidade_etaria.csv") %>% 
  pivot_longer(X.0.1.:Desconhecido, "Grupo") %>% 
  mutate(Grupo = case_when(Grupo == "X.0.1." ~ "0-1",
                           Grupo == "X.1.4." ~ "1-4",
                           Grupo == "X.5.14." ~ "5-14",
                           Grupo == "X.15.24." ~ "15-24",
                           Grupo == "X.25.34." ~ "25-34",
                           Grupo == "X.35.44." ~ "35-44",
                           Grupo == "X.45.54." ~ "45-54",
                           Grupo == "X.55.64." ~ "55-64",
                           Grupo == "X.65.74." ~ "65-74",
                           Grupo == "X.75.84." ~ "75-84",
                           Grupo == "X.85.." ~ "85+", 
                           TRUE ~ "unknown"))

# mortes para a população sénior (65+)
mort_65<-mort_et %>% mutate(plus.65=case_when(Grupo == "65-74" | Grupo == "75-84" | Grupo == "85+" ~ 1,
                                              TRUE ~ 0)) %>% 
  group_by(ano, plus.65) %>% 
  summarise(n.mort = sum(value)) %>% ungroup()

# dados demograficos em formato longo
pop<-read.csv2("data/demog.csv") %>% 
  pivot_longer(4:21, "g.etario", "n") %>% 
  mutate(plus.65 = case_when(g.etario == "g.65.69" | g.etario == "g.70.74" | 
                               g.etario == "g.75.79" | g.etario == "g.80.84" | 
                               g.etario == "g.85." ~ 1,
                             TRUE ~ 0))
# população com mais de 65 anos
pop_65<-pop %>% filter(ano > 1999) %>% group_by(ano,plus.65) %>% 
  summarise(n.pop = sum(value)) %>% ungroup()


################################

gdh<-read.csv2("data/gdh_internamentos.csv")

precos2009<-read_csv2("data/precos2009.csv")
precos2013<-read_csv2("data/precos2013.csv")

gdh <- gdh %>% left_join(precos2009) %>% left_join(precos2013)

gdh <- gdh %>% mutate(v2000 = n2000 * preco2009,
                      v2001 = n2001 * preco2009,
                      v2002 = n2002 * preco2009,
                      v2003 = n2003 * preco2009,
                      v2004 = n2004 * preco2009,
                      v2005 = n2005 * preco2009,
                      v2006 = n2006 * preco2009,
                      v2007 = n2007 * preco2009,
                      v2008 = n2008 * preco2009,
                      v2009 = n2009 * preco2009,
                      v2010 = n2010 * preco2009,
                      v2011 = n2011 * preco2009,
                      v2012 = n2012 * preco2009,
                      v2013 = n2013 * preco2013,
                      v2014 = n2014 * preco2013
                      )

gdh <- gdh %>% filter(!gdh %in% c(342,343,415,483)) %>%  
  rowwise() %>% mutate(avg = mean(v2000:v2014, na.rm = T)) %>% ungroup()

gdh10<-gdh %>% top_n(10, avg) %>% select(gdh, v2000:v2014) %>% 
  pivot_longer(v2000:v2014, names_to = "vano") %>% 
  mutate(ano = as.numeric(str_sub(vano, start= -4)))

