# Title     : TODO
# Objective : TODO
# Created by: W
# Created on: 2020/10/14

library(tidyverse)
library(readxl)
library(wbstats)
library(psych)
library(janitor)
library(plm)
library(stargazer)

# Load China Global Investment Datasets

cgit_1 <- read_excel("data/cgit_2020s.xlsx" , "Dataset 1"  , skip = 4)
cgit_2 <- read_excel("data/cgit_2020s.xlsx" , "Dataset 2"  , skip = 4)
cgit_3 <- read_excel("data/cgit_2020s.xlsx" , "Dataset 3"  , skip = 4)
cgit_12 <- read_excel("data/cgit_2020s.xlsx", "Dataset 1+2", skip = 4)

cgit_investments <- cgit_1 %>%
  group_by(Country, Year) %>%
  summarize(cgit_investments = sum(`Quantity in Millions`))

cgit_construction <- cgit_2 %>%
  group_by(Country, Year) %>%
  summarize(cgit_construction = sum(`Quantity in Millions`))

cgit_troubled <- cgit_3 %>%
  group_by(Country, Year) %>%
  summarize(cgit_troubled = sum(`Quantity in Millions`))

cgit_total <- cgit_12 %>%
  group_by(Country, Year) %>%
  summarize(cgit_total = sum(`Quantity in Millions`))

cgit_green <- cgit_12 %>%
  filter(Greenfield == 'G') %>%
  group_by(Country, Year) %>%
  summarize(cgit_green = sum(`Quantity in Millions`))

# mapping of CGIT country to ISO3 code
cgit_iso3 <- read_csv("data/cgit_iso3.csv")

# join them all
cgit_all <- cgit_investments %>%
  full_join(cgit_construction, by = c("Country", "Year")) %>%
  full_join(cgit_troubled, by = c("Country", "Year")) %>%
  full_join(cgit_total, by = c("Country", "Year")) %>%
  full_join(cgit_green, by = c("Country", "Year")) %>%
  left_join(cgit_iso3, by = c("Country" = "country"))


# Download World Development Indicators

if (file.exists("data/wb_data.csv")) {

  # load from cache if available
  wb_data <<- read_csv("data/wb_data.csv")

} else {

  wb_vars <- list(
    "NY.GDP.PCAP.KN"       = "gdp_lcu",         # GDP per capita (constant LCU)
    "NY.GDP.PCAP.KD"       = "gdp_usd",         # GDP per capita (constant 2010 US$)
    "NY.GDP.PCAP.PP.KD"    = "gdp_ppp",         # GDP per capita, PPP (constant 2017 international $)
    "NY.GDP.MKTP.CD"       = "gdp",             # GDP (current US$)
    "NY.GDP.PCAP.CD"       = "gdp_pc",          # GDP per capita (current US$)
    "NY.GDP.PCAP.KD.ZG"    = "gdp_gr",          # GDP per capita growth (annual %)
    "NY.GDS.TOTL.ZS"       = "savings",         # Gross domestic savings (% of GDP)
    "BX.KLT.DINV.WD.GD.ZS" = "fdi",             # Foreign direct investment, net inflows (% of GDP)
    "NE.TRD.GNFS.ZS"       = "trade",           # Trade (% of GDP)
    "FS.AST.PRVT.GD.ZS"    = "credit",          # Domestic credit to private sector (% of GDP)
    "NY.GDP.TOTL.RT.ZS"    = "rents",           # Total natural resources rents (% of GDP)
    "SP.POP.TOTL"          = "pop",             # Population, total
    "SE.SEC.ENRR"          = "school",          # School enrollment, secondary (% gross)
    "PA.NUS.FCRF"          = "fx_rate",         # Exchange rate (LCU per US$, period average)
    "DT.ODA.ODAT.GN.ZS"    = "oda",             # Net ODA received (% of GNI)
    "VA.PER.RNK"           = "wgi_voic",        # Voice and Accountability: Percentile Rank
    "RL.PER.RNK"           = "wgi_rule",        # Rule of Law: Percentile Rank
    "RQ.PER.RNK"           = "wgi_regl",        # Regulatory Quality: Percentile Rank
    "PV.PER.RNK"           = "wgi_poli",        # Political Stability/No Violence: Percentile Rank
    "GE.PER.RNK"           = "wgi_effi",        # Government Effectiveness: Percentile Rank
    "CC.PER.RNK"           = "wgi_corr"         # Control of Corruption: Percentile Rank
  )

  # fetch data online from World Bank
  wb_data <<-
    wb(indicator = names(wb_vars), startdate = 2005, enddate = 2019) %>%
      mutate(var = wb_vars[indicatorID]) %>%
      select(country, iso3c, date, var, value) %>%
      pivot_wider(names_from = "var", values_from = "value") %>%
      arrange(iso3c, date)

  write_csv(wb_data, "data/wb_data.csv")

}


# Load country classification
un_country <- read_excel("data/jme_2017.xlsx") %>% clean_names()

# join all data into one table
full_sample <- wb_data %>%
  left_join(cgit_all, by = c("iso3c" = "iso3", "date" = "Year")) %>%
  left_join(un_country, by = c("iso3c" = "iso3_code")) %>%
  mutate_at(vars(contains("cgit")), ~replace_na(.,0)) %>%
  drop_na(`country_name`) %>%
  select(-Country, -country_name)

# write_csv(full_sample, "data/sample.csv")


# creats a sample of only low- and middle-income countries
subsample <- full_sample %>%
  filter(world_bank_income_group_combined_1 != "High Income",
         country != "China") %>%
  mutate(cgit_total = cgit_total*10^8 / gdp,
         cgit_investments = cgit_investments*10^8 / gdp,
         cgit_construction = cgit_construction*10^8 / gdp,
         cgit_green = cgit_green*10^8 / gdp,
         wgi_overall = wgi_corr + wgi_effi + wgi_poli + wgi_regl + wgi_rule + wgi_voic,
         post_bri = date > 2013)


# REGRESSION ANALYSIS

plm.1 <- plm(gdp_gr ~ cgit_total + plm::lag(cgit_total, 1) + plm::lag(cgit_total, 2) +
             log(gdp_pc) + trade + credit  + rents + wgi_overall + school + oda,
    data   = subsample,
    index  = c("iso3c", "date"),
    model  = "within",
    effect = "twoways")

plm.2 <- plm(gdp_gr ~ cgit_construction + plm::lag(cgit_construction, 1) + plm::lag(cgit_construction, 2) +
  log(gdp_pc) + trade + credit + rents + wgi_overall + school + oda,
    data   = subsample,
    index  = c("iso3c", "date"),
    model  = "within",
    effect = "twoways")

plm.3 <- plm(gdp_gr ~ cgit_investments + plm::lag(cgit_investments, 1) + plm::lag(cgit_investments, 2) +
  log(gdp_pc)  + trade + credit + rents + wgi_overall + school + oda,
    data   = subsample,
    index  = c("iso3c", "date"),
    model  = "within",
    effect = "twoways")

plm.4 <- plm(gdp_gr ~ cgit_green + plm::lag(cgit_green, 1) + plm::lag(cgit_green, 2) +
  log(gdp_pc) + trade + credit + rents + wgi_overall + school + oda,
    data   = subsample,
    index  = c("iso3c", "date"),
    model  = "within",
    effect = "twoways")

stargazer(plm.1, plm.2, plm.3, plm.4, type = "text", out = "table/panel1.txt")

plm.5 <- plm(gdp_gr ~ plm::lag(cgit_total, 1):factor(un_regions) +
  log(gdp_pc) + trade + credit  + rents + wgi_overall + school + oda,
             data   = subsample,
             index  = c("iso3c", "date"),
             model  = "within",
             effect = "twoways")

stargazer(plm.5, type = "text", out = "table/panel2.txt")

plm.6 <- plm(gdp_gr ~ plm::lag(cgit_green, 1):factor(subsample$world_bank_income_group_combined_1) +
  log(gdp_pc) + trade + credit + rents + wgi_overall + school + oda,
             data   = subsample,
             index  = c("iso3c", "date"),
             model  = "within",
             effect = "twoways")

stargazer(plm.6, type = "text", out = "table/panel3.txt")


# DEBT ANALYSIS

# Total debt service (% of exports of goods, services and primary income)
debt_export <- wb(indicator = "DT.TDS.DECT.EX.ZS") %>%
    mutate(date = as.integer(date))

# Latin America & Caribbean (excluding high income)
# Sub-Saharan Africa (excluding high income)
debt_export %>%
  filter(iso3c == 'LAC' | iso3c == 'SSA') %>%
  ggplot(aes(x = date, y = value, group = iso3c)) +
  labs(x = "", y = "Total debt service ratio") +
  geom_line() + facet_grid(~iso3c)

# External debt stocks (% of GNI)
debt_gni <- wb(indicator = "DT.DOD.DECT.GN.ZS") %>%
    mutate(date = as.integer(date))

debt_gni %>%
  filter(iso3c == 'LAC' | iso3c == 'SSA') %>%
  ggplot(aes(x = date, y = value, group = iso3c)) +
  labs(x = "", y = "External debt stock (% of GNI)") +
  geom_line() + facet_grid(~iso3c)


# China loan to African countries
china_loan <-
  read_excel("data/loan_2020.xlsx",
             sheet = "Loan data by country",
             range = "A5:BF24") %>%
    pivot_longer(cols      = 2:58,
                 names_to  = "country",
                 values_to = "amount") %>%
    select(date = `All Type 1 Loans`, everything())

all_loan <-
  wb_data %>% select(country, date, gdp) %>%
    inner_join(china_loan, by = c("country", "date")) %>%
    inner_join(debt_gni, by = c("country", "date")) %>%
    mutate(china = amount*10^8 / gdp, all_external = value) %>%
    filter(date == 2018, china > 0) %>%
    select(country, china, all_external) %>%
    pivot_longer(cols = china:all_external)

# China loan as proportion as total external debt 
all_loan %>%
  ggplot(aes(x = country, y = value, fill = name)) + 
  geom_col() + 
  scale_fill_manual(
    name = NULL, 
    labels = c("Total", "From China"), 
    values = c("grey","black")) + 
  labs(x = "", y = "External debt (% of GNI)") +
  theme(axis.text.x = element_text(angle = 90))




