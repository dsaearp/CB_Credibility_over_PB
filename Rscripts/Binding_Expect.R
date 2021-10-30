library(xlsx)
library(dplyr)
library(tidyr)
library(data.table)
library(lubridate)

setwd("C:/Users/dsaearp/Desktop/pasta_R_teste/MM")

# Creating a Function to Organize Expectations Data Base

gen_database <- function(path){
  
  b1 <- fread(path, header = T)
  
  
  b1 <- b1 %>% pivot_longer(cols = c(2:ncol(b1)), names_to = "Horizon", values_to = "Forecast")
  
  b1 <- b1 %>% filter(!is.na(Forecast)) %>% mutate(Horizon = Horizon %>% as.integer(),
                                                   Dates = Dates %>% dmy(),
                                                   Ref = paste0("1.", month(Dates),".",year(Dates)))
  
  bb <- b1 %>% group_by(Ref, Horizon) %>%
    group_modify(
      ~ .x %>% mutate(Mean = mean(Forecast))
    ) %>% ungroup()
  
  bb <- bb %>% select(Ref, Horizon, Mean) %>% unique()
  
  bb <- bb %>% mutate(Ref = Ref %>% dmy())
  
  return(bb)
  
}

# Reading Expectations Data Base _______________________________________________

# Source: https://www3.bcb.gov.br/expectativas2/#/consultaSeriesEstatisticas

# 2000 - 2007

b1 <- gen_database("2000_2007.csv")

# 2008 - 2017

b2 <- gen_database("2008_2017.csv")

# 2018 Until 01-01-21

b3 <- gen_database("2018_2021_10.csv")

base <- rbind(b1, b2, b3) %>% arrange(Ref)

# Getting Inflation Target History ____________________________________________

if_target <- fread("Inflation_Targets.csv") 

if_target <- if_target %>% mutate(Establish_Date = Establish_Date %>% dmy())

# Merging two datasets

# 2003 and 2004 had target revised impacting forecasts

# See for yourself
ggplot(data = base %>% filter(Horizon == 2003))+
  geom_line(aes(x = Ref, y = Mean))+
  geom_vline(xintercept = "28/6/2001" %>% dmy(), color = "red")+
  geom_hline(yintercept = 3.25, color = "red", linetype = 2)+
  geom_vline(xintercept = "27/6/2002" %>% dmy(), color = "blue")+
  geom_hline(yintercept = 4, color = "blue", linetype = 2)+
  geom_vline(xintercept = "21/1/2003" %>% dmy(), color = "dark green")+
  geom_hline(yintercept = 8.5, color = "dark green", linetype = 2)

# Adjusting for that!

base1 <- base %>% filter(!(Horizon %in% c(2003, 2004)))

base1 <- base1 %>% left_join(if_target, by = "Horizon")

base21 <- base %>% filter(Horizon == 2003) %>%
  mutate(Establish_Date = ifelse(Ref < "28/6/2001" %>% dmy(),  "28/6/2001",
                                 ifelse(Ref < "27/6/2002" %>% dmy(), "27/6/2002",
                                        "21/1/2003"))) %>%
  mutate(Establish_Date = Establish_Date %>% dmy())

base22 <- base %>% filter(Horizon == 2004) %>%
  mutate(Establish_Date = ifelse(Ref < "27/6/2002" %>% dmy(), "27/6/2002",
                                 "21/1/2003"))%>%
  mutate(Establish_Date = Establish_Date %>% dmy())

base2 <-  rbind(base21, base22) %>% left_join(if_target)

base_final <- rbind(base1, base2)

write.csv(base_final, "Expect_DF.csv")
  

# Graphs =======================================================================




bb %>%
  ggplot(aes(x = Desvios, y = Indexados))+
  geom_point(aes(color = Periodo))+
  geom_smooth(method = 'lm', se = F, color = "red", data = bb %>% filter(Ano == 2016))+
  geom_smooth(method = 'lm', se = F, data = bb %>% filter(Ano != 2016))+
  theme_light()+
  labs(title = "BCB sob Goldfajn")+
  ylab("Indexados (%)")+
  xlab("Desvios em relação à meta")

