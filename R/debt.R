# Title: debt.R
# Objective : Plot debt situations in some regions
# Created by: W
# Created on: 2020/10/28

library(tidyverse)
library(wbstats)
library(readxl)
library(janitor)

# the following code is using wbstats 1.0.1

indicators <- c(
  gdp = "NY.GDP.MKTP.CD", # GDP (current US$)
  gdp_pc = "NY.GDP.PCAP.CD", # GDP per capita (current US$)
  debt_ext = "DT.DOD.DECT.CD", # External debt stock (USD)
  debt_ext_srv = "DT.TDS.DECT.CD", # External debt service (USD)
  debt_mul_srv = "DT.TDS.MLAT.CD", # Multilateral debt service (USD)
  rents_gdp = "NY.GDP.TOTL.RT.ZS",  # Total natural resources rents (% GDP)
  exports = "NE.EXP.GNFS.CD" # Exports of goods and service
  
)

data <- wb_data(indicators, country = "all") 


# prepares variables for plotting

data_1 <- data %>%
  filter(iso3c %in% c("SSA","LAC","SAS"), date > 1970) %>%
  mutate(region = case_when(iso3c == "SSA" ~ "Sub-Saharan Africa", 
                            iso3c == "LAC" ~ "Latin America", 
                            TRUE ~ country )) %>%
  mutate(rents = rents_gdp*gdp/100) %>%
  mutate(`debt_gdp` = debt_ext/gdp*100, 
         debt_export = debt_ext_srv/exports*100, 
         debt_rents = debt_ext_srv/rents*100) %>%
  select(region, date, 
         `(1) External debt / GDP` = debt_gdp, 
         `(2) Debt service / Exports` = debt_export, 
         `(3) Debt service / Rents` = debt_rents, 
         `(4) Resource rents / GDP` = rents_gdp) %>%
  pivot_longer(cols = 3:6)

ggplot(data_1, aes(date, value)) + geom_line(size=1) + 
  facet_grid(name~region, scales = "free_y") + 
  labs(x = NULL, y = NULL)


# China loan to Africa, 2000-2018

loan <- read_excel(path = "data/loan_2020.xlsx", 
                   sheet = "Loan data by financier-country", 
                   range = "A2:F57") %>% clean_names()

countries <- wb_countries()

# no important countries missing by inner joining
# except DRC (Congo, Dem. Rep.) and Egypt (Egypt, Arab Rep.)

loan_1 <- loan %>% inner_join(countries, "country") 

loan_2 <- data %>% 
  filter(date == 2018) %>% 
  inner_join(loan_1, "iso3c") %>% 
  mutate(loan_gdp = total*10^6/gdp*100, 
         debt_gdp = debt_ext/gdp*100) %>%
  filter(debt_gdp > 0, loan_gdp > 0)

ggplot(loan_2) + 
  geom_col(aes(iso3c, debt_gdp), alpha = .5) + 
  geom_col(aes(iso3c, loan_gdp), alpha = 1) + 
  labs(x = NULL, y = "China loan vs. External debt / GDP") + 
  theme(axis.text.x = element_text(angle = 90, vjust=0))


