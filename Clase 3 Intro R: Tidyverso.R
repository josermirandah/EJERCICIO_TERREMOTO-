 library(tidyverse)

 Contagiados <- read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto19/CasosActivosPorComuna.csv")

 Contagiados2 <- Contagiados %>% 
   pivot_longer(cols = starts_with(c("2020","2021","2022"))  ,
                names_to = "Fecha",
                values_to = "Infectados") %>% 
   mutate(Fecha = lubridate::ymd(Fecha), prevalencia = 100000*(Infectados/Poblacion)) %>% 
   dplyr::filter(Fecha == min(Fecha), !is.na(`Codigo comuna`), Region == "Metropolitana") %>% 
   arrange(desc(prevalencia)) %>% group_by(Region,Comuna) %>% 
  
   Contagiados3 <- Contagiados %>% 
   pivot_longer(cols = starts_with(c("2020","2021","2022"))  ,
                names_to = "Fecha",
                values_to = "Infectados") %>% 
   mutate(Fecha = lubridate::ymd(Fecha), prevalencia = 100000*(Infectados/Poblacion)) %>% 
   dplyr::filter( !is.na(`Codigo comuna`), Region == "Metropolitana") %>% group_by(Region,Comuna) %>% 
   summarise(med= median(prevalencia),MAX_PREV= max(prevalencia),DESV= sd(prevalencia)) 
 
 
 arrange(Contagiados3,MAX_PREV)
 
 
 str(Contagiados2)
 
 
 
 