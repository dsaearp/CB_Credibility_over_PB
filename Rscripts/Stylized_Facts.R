library(xlsx)
library(dplyr)
library(tidyr)
library(data.table)
library(lubridate)
library(ggplot2)

setwd("C:/Users/dsaearp/Desktop/pasta_R_teste/MM")

# Reading Data Frames ==========================================================

Indexed_raw <- fread("Debt_Composition.csv") %>% rename("Ref" = "Dates") %>%
  mutate(Ref = Ref %>% mdy())

Indexed <- fread("Debt_Composition.csv") %>% rename("Ref" = "Dates") %>%
  mutate(Ref = Ref %>% mdy())

# Available at https://www.bcb.gov.br/content/estatisticas/Documents/Tabelas_especiais/DivMobp.xls

ii <- xlsx::read.xlsx('DivMobp.xls', sheetIndex = 3)  

colnames(ii) <- c('Year', 'Month', 'Stock', 'Selic_ex_swap', 'Selic_swap', 
                  'Exchange_Rate_ex_swap', 'Exchange_Rate_swap', 'Nominal', 'TR',
                  'Price_Index', 'Others', 'Open_Mkt_Operations', 'Total')

ii2 <- ii %>% select(!Year)

ii2 <- ii2 %>% na.exclude()

ii2 <- ii2 %>% mutate(Ref = seq.Date(from = ymd("1994-07-01"), to = ymd("2021-08-01"), by = 'month'),
                      Indexed = as.numeric(Selic_ex_swap) + as.numeric(Price_Index) + as.numeric(Exchange_Rate_ex_swap),
                      Nominal = as.numeric(Nominal))

ii_raw <- ii2

ii2 <- ii2 %>% select(Ref, Indexed, Nominal)

Indexed <- ii2


# Creating Presidents Data Frame

Ar <- data.table(Ref = seq.Date(from ="1999-03-01" %>% ymd(), to = "2002-12-01" %>% ymd(), by = "month")) %>%
  mutate(President = "Arminio")

Mei <- data.table(Ref = seq.Date(from ="2003-03-01" %>% ymd(), to = "2010-12-01" %>% ymd(), by = "month")) %>%
  mutate(President = "Meirelles")

Tombini <- data.table(Ref = seq.Date(from ="2011-01-01" %>% ymd(), to = "2016-05-01" %>% ymd(), by = "month")) %>%
  mutate(President = "Tombini")

Goldfajn <- data.table(Ref = seq.Date(from ="2016-06-01" %>% ymd(), to = "2019-02-01" %>% ymd(), by = "month")) %>%
  mutate(President = "Goldfajn")

RCN <- data.table(Ref = seq.Date(from ="2019-03-01" %>% ymd(), to = "2021-12-01" %>% ymd(), by = "month")) %>%
  mutate(President = "RCN")

Presidents <- rbind(Ar, Mei, Tombini, Goldfajn, RCN)
write.csv(Presidents, "Presidents.csv")
rm(Ar, Mei, Tombini, Goldfajn, RCN)
#Available at: https://www.bcb.gov.br/pre/galeriadospresidentes/default-p.asp?frame=1

base <- left_join(Indexed, Presidents)

# Expectations 

Expect <- fread("Expect_DF.csv")

Expect <- Expect %>% mutate(Short_Dev = Mean - Target,
                            Ref = Ref %>% mdy())

Expect <- Expect %>% group_by(Ref) %>%
  group_modify(~ {
    .x %>% mutate(Deviation = mean(Short_Dev, na.rm = T))
  }) %>% ungroup()

Expect <- Expect %>% select(Ref, Deviation) %>% 
  arrange(Ref) %>% unique() 


base <- left_join(base, Expect) %>% rename("Datas" = "Ref")

# Graphs =======================================================================

bb <- base %>% select(Datas, Indexed, Nominal) %>%
  rename("Indexados" = "Indexed") %>%
  pivot_longer(cols = c(2,3), names_to = "Type", values_to = "Share")

bb %>%
  ggplot(aes(x = Datas, y = Share, color = Type))+
  geom_line()+
  theme_light()+
  labs(title = "Evolução da Composição dos Títulos (%)")+
  scale_x_date(breaks = scales::pretty_breaks(n = 15))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.title = element_blank())

ggsave("Charts/composicao.png", height=5, width=6)



bb2 <- ii_raw %>% mutate(Ref = Ref %>% ymd()) %>%
  rename("Indice de Preços" = "Price_Index",
         "Selic" = "Selic_ex_swap") %>%
  select(Ref, Selic, `Indice de Preços`)

bb2 <- bb2 %>% pivot_longer(cols = c(2,3), names_to = "Type", values_to = "Share")

bb2 <- bb2 %>% mutate(Share = as.numeric(Share))

bb2 %>%
  ggplot(aes(x = Ref, y = Share, color = Type))+
  geom_line()+
  theme_light()+
  labs(title = "Evolução da Composição dos Títulos (%)")+
  scale_x_date(breaks = scales::pretty_breaks(n = 15))+
  theme(legend.position = "bottom", axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.title = element_blank())

ggsave("Charts/Indexed_composicao.png", height=5, width=6)



base %>% filter(President == "Meirelles") %>%
  ggplot(aes(x = Deviation, y = Indexed))+
  geom_point(aes(color = Datas))+
  geom_smooth(method = 'lm', se = F)+
  theme_light()+
  labs(title = "BCB sob Meirelles")+
  ylab("Indexados (%)")+
  xlab("Desvios em relação à meta (p.p.)")

ggsave("Charts/bcb_sob_Mei.png", height=5, width=6)

base %>% filter(President == "Tombini") %>%
  ggplot(aes(x = Deviation, y = Indexed))+
  geom_point(aes(color = Datas))+
  geom_smooth(method = 'lm', se = F)+
  theme_light()+
  labs(title = "BCB sob Tombini")+
  ylab("Indexados (%)")+
  xlab("Desvios em relação à meta (p.p.)")

ggsave("Charts/bcb_sob_Tomb.png", height=5, width=6)

bb <-  base %>% filter(President == "Goldfajn") %>%
  mutate(Periodo = ifelse(Datas <= "2017-06-01" %>% ymd(), "Primeiro Ano", "Resto do Mandato"))

bb %>%
  ggplot(aes(x = Deviation, y = Indexed))+
  geom_point(aes(color = Datas))+
  geom_smooth(method = 'lm', se = F, color = "red", data = bb %>% filter(Datas <= ymd("2017-06-01")))+
  geom_smooth(method = 'lm', se = F, data = bb %>% filter(Datas >= ymd("2017-06-01")))+
  theme_light()+
  labs(title = "BCB sob Goldfajn")+
  ylab("Indexados (%)")+
  xlab("Desvios em relação à meta (p.p.)")

ggsave("Charts/bcb_sob_Gold.png", height=5, width=6)

base %>% filter(President == "RCN") %>%
  ggplot(aes(x = Deviation, y = Indexed))+
  geom_point(aes(color = Datas))+
  geom_smooth(method = 'lm', se = F)+
  theme_light()+
  labs(title = "BCB sob Campos Neto")+
  ylab("Indexados (%)")+
  xlab("Desvios em relação à meta (p.p)")

ggsave("Charts/bcb_sob_RCN.png", height=5, width=6)
