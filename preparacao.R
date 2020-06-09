gdh<-read.csv2("data/gdh_internamentos.csv")

precos2009<-read_csv2("data/precos2009.csv")
#precos2013<-read_csv2("data/precos2013.csv")

gdh <- gdh %>% left_join(precos2009)

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
                      v2013 = n2013 * preco2009,
                      v2014 = n2014 * preco2009,
                      v2015 = n2015 * preco2009,
                      v2016 = n2016 * preco2009
                      )

gdh <- gdh %>% rowwise() %>%
  mutate(avgV = mean(v2000:v2016, na.rm = T),
         avgN = mean(n2000:n2016, na.rm = T)) %>% ungroup()

gdh10V<-gdh %>% top_n(11, avgV) %>% select(gdh, v2000:v2016) %>%
  pivot_longer(v2000:v2016, names_to = "vano") %>%
  mutate(ano = as.numeric(str_sub(vano, start= -4)))

write.csv(gdh10V,"data/gdh10V.csv")

gdh10N<-gdh %>% top_n(11, avgN) %>% select(gdh, n2000:n2016) %>%
  pivot_longer(n2000:n2016, names_to = "nano") %>%
  mutate(ano = as.numeric(str_sub(nano, start= -4)))

write.csv(gdh10N,"data/gdh10N.csv")






