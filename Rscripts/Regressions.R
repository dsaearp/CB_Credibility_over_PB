# Regressions

library(data.table)
library(dplyr)
library(stargazer)
library(lubridate)
library(mlr)

setwd("C:\\Users\\dsaearp\\Desktop\\pasta_R_teste\\MM")

# Reading Data

Di_IPCA <- fread('DI_IPCA_1Y.csv')
Di_IPCA <- Di_IPCA %>% mutate(Data = Data %>% ymd()) %>% arrange(Data)

Di_IGPM <- fread('DI_IGPM.csv')
Di_IGPM <- Di_IGPM %>% mutate(Data = Data %>% ymd()) %>% arrange(Data)

Pre_LTN <- fread('Pre_LTN.csv')
Pre_LTN <- Pre_LTN %>% mutate(Data = Data %>% ymd())

Di_Pre <- fread('Di_Pre.csv')
Di_Pre <- Di_Pre %>% mutate(Data = Data %>% ymd())

IPCA_12M <- fread('IPCA_12M.csv') %>% 
  mutate(Data = Data %>% ymd(),
         IPCA_12meses = IPCA_12meses %>% as.numeric()) 

Presidents <- fread('Presidents.csv') %>% rename("Data" = "Ref") %>%
  mutate(Data = Data %>% ymd()) %>% select(Data, President)


Expect_DF <- fread('Expect_DF.csv') %>% rename("Data" = "Ref", "Expect" = "Mean") %>%
  mutate(Data = Data %>% mdy()) %>% 
  select(Data, Horizon ,Expect) 

Expect_DF <- Expect_DF %>% mutate(Next_Year = ifelse(Horizon == year(Data) + 1, T, F))

Expect_DF <- Expect_DF %>% filter(Next_Year)

Expect_DF <- Expect_DF %>% select(Data, Expect)

ii <- xlsx::read.xlsx('DivMobp.xls', sheetIndex = 3)  

colnames(ii) <- c('Year', 'Month', 'Stock', 'Selic_ex_swap', 'Selic_swap', 
                  'Exchange_Rate_ex_swap', 'Exchange_Rate_swap', 'Nominal', 'TR',
                  'Price_Index', 'Others', 'Open_Mkt_Operations', 'Total')

ii2 <- ii %>% select(!Year)

ii2 <- ii2 %>% na.exclude()

ii2 <- ii2 %>% mutate(Ref = seq.Date(from = ymd("1994-07-01"), to = ymd("2021-08-01"), by = 'month'),
                      Indexed = as.numeric(Selic_ex_swap) + as.numeric(Price_Index) + as.numeric(Exchange_Rate_ex_swap),
                      Nominal = as.numeric(Nominal))

Debt_Comp <- ii2 %>% select(Ref, Indexed, Nominal) %>%
  rename("Data" = "Ref")



# Merging

Base <- left_join(Di_IGPM, Di_Pre) %>%
  left_join(Di_IPCA) %>% left_join(Pre_LTN) %>%
  left_join(Presidents) %>% left_join(Expect_DF) %>%
  left_join(Debt_Comp) %>% left_join(IPCA_12M)


Base <- Base %>% mutate(Month = format(Data, "%b"))

Dummy_Month <- cbind(Base$Data, createDummyFeatures(Base$Month, cols = "var"))

colnames(Dummy_Month)[1] <- "Data"

Presidents_dummy <- cbind(Base$Data, createDummyFeatures(Base$President, cols = 'var'))

colnames(Presidents_dummy)[1] <- "Data"

Base <- left_join(Base, Dummy_Month) %>% left_join(Presidents_dummy)

Base <- Base %>% filter(Data <= "2021-08-01")


# Regressions

# Di_Pre x IGP-M
Base1 <- Base %>% mutate(implicit_inflation = DI_Pre - Di_IGPM)
Base1 <- Base1 %>% mutate(inflation_premium = implicit_inflation - Expect)


m1 <- lm(Nominal ~ inflation_premium + Meirelles + Tombini + Goldfajn + Feb + Mar + May + May + Jun + Jul + Aug +
          Sep + Oct + Nov + Dec, data = Base1)

summary(m1)


# Com IPCA

m12 <- lm(Nominal ~ inflation_premium + IPCA_12meses + Meirelles + Tombini + Goldfajn + Feb + Mar + May + May + Jun + Jul + Aug +
           Sep + Oct + Nov + Dec, data = Base1)

summary(m12)

stargazer(m1, m12, type = 'html',  omit = c("Feb", "Mar", "May", "May",
                                        "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), out = "models1.htm")


# LTN x IGP-M

Base2 <- Base %>%  mutate(implicit_inflation = Pre_LTN_12m - Di_IGPM)
Base2 <- Base2 %>% mutate(inflation_premium = implicit_inflation - Expect)

m2 <- lm(Nominal ~ inflation_premium + Meirelles + Tombini + Goldfajn + Feb + Mar + May + May + Jun + Jul + Aug +
           Sep + Oct + Nov + Dec, data = Base2)

summary(m2)

stargazer(m2, type = 'html', out = "models2.htm")


# Com IPCA

m22 <- lm(Nominal ~ inflation_premium + IPCA_12meses + Meirelles + Tombini + Goldfajn + Feb + Mar + May + May + Jun + Jul + Aug +
            Sep + Oct + Nov + Dec, data = Base2)

summary(m22)

stargazer(m2, m22, type = 'html', omit = c("Feb", "Mar", "May", "May",
                                           "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), out = "models2.htm")
