#' =============================================================
#'
#' PROJECT:
#'     Belt & Road Initiative Assesment
#'
#' SCRIPT PURPOSE:
#'     Data manupulation related scripts.
#'
#' AUTHOR: Z.
#'
#' DATE: 1 APRIL 2020
#'
#' NOTICE:  All information contained herein is, and remains
#' the property of the author. No dissemination or reproduction
#' of this information is permitted prior permission is obtained
#' from the author.
#'
#' =============================================================

library(rstudioapi)
library(tidyverse)
library(wbstats)
library(MatchIt)


# set working directory to source file location
setwd(dirname(getActiveDocumentContext()$path))



# =============== Load country profile ======================

country <- read_csv("../data/country.csv", col_types = "ffffccfiiiiii")

bri     <- read_csv("../data/bri.csv",     col_types = "cciii")


# ============== Load investment profile ====================

if (file.exists("../data/invest.csv")) {

  invest <<-
    read_csv("../data/invest.csv", col_types = "cinnnl") %>%
    replace_na(list(BRI_shock = F))

} else {

  # CGIT: China Global Investment Tracker
  cgi <- read_csv("../data/cgi.csv", col_types = "ifcnncffccfi")

  # I delibrately keep a different variable name to avoid override.
  # TODO: This needs to be refactored later on.
  cgi_year <<-
    cgi %>%
    group_by(Code, Year) %>%
    summarise(
      # CGI_sector     = str_c(Sector, collapse = ","),
      # CGI_contractor = str_c(Contractor, collapse = ","),
      # CGI_region     = last(Region),
      Invt_count       = n(),
      Invt_quant       = sum(`Quantity in Millions`),
      Invt_nBRI        = sum(BRI, na.rm = T)
    ) %>%
    mutate(
      BRI_shock = imap_int(
        Invt_nBRI,
        function(n, ind, inv) {
          # the first time receiving a B&R investment is considered a shock
          if (n > 0 & ind == 1 & inv[[ind]] > 800) return(1L)
          # if a country has received investment before the BRI program
          # a BRI shock is identified if the amount of investment after BRI
          # is significantly greater than any previous investments
          else if (n > 0 & inv[[ind]] > max(800, 1.2 * max(tail(inv[1:(ind - 1)], 2)))) return(1L)
          else return(NA)
        },
        Invt_quant
      )
    ) %>%
    arrange(CGI_region, Code)

  write_csv(cgi_year, "../data/cgi_year.csv", na = '')
}



# ========================== Load data from World Bank  ===============================


if (file.exists("../data/wdi.csv")) {

  # load from cache if available
  wdi <<- read_csv("../data/wdi.csv", col_types = cols(Year = col_integer(),
                                                       Code = col_character()))

} else {

  wb_ids <-
    list(
      "SP.POP.TOTL"       = "Pop"         , # Population, total
      "EN.POP.DNST"       = "Pop_dnst"    , # Population density (people per sq. km)
      "NY.GDP.MKTP.KD"    = "KGDP"        ,	# GDP (constant 2010 US$)
      "NY.GDP.PCAP.KD"    = "KGDP_pc"     ,	# GDP per capita (constant 2010 US$)
      "NY.GDP.MKTP.CD"    = "GDP"         ,	# GDP (current US$)
      "NY.GDP.PCAP.CD"    = "GDP_pc"      ,	# GDP per capita (current US$)
      "NY.GDP.MKTP.KD.ZG" = "GDP_growth"  ,	# GDP growth (constant local currency)
      "SL.UEM.TOTL.NE.ZS" = "Uem_rate"    ,	# Unemployment (% of total labor force)
      "NV.AGR.TOTL.ZS"    = "Agr_share"   ,	# Agriculture value added (% of GDP)
      "NV.IND.TOTL.ZS"    = "Ind_share"   ,	# Industry (inc. construction) (% of GDP)
      "NE.TRD.GNFS.ZS"    = "Trd_share"   ,	# Trade (% of GDP)
      "GC.XPN.TOTL.GD.ZS" = "Exp_share"   , # Expense (% of GDP)
      "BX.GSR.MRCH.CD"    = "Exports"     ,	# Goods exports (BoP, current US$)
      "BM.GSR.MRCH.CD"    = "Imports"     ,	# Goods imports (BoP, current US$)
      "PA.NUS.FCRF"       = "FX_rate"	    , # Exchange rate (LCU per US$, period average)
      "IS.RRS.TOTL.KM"    = "Rail"        , # Rail lines (total route-km)
      "LP.LPI.INFR.XQ"    = "LPI_infr"    , # Logistics performance index: transport
      "DT.ODA.ODAT.KD"    = "Aid"         , # Net ODA received (constant 2015 US$)
      "SE.SCH.LIFE"       = "School"      , # School life expectancy, primary to tertiary
      "SE.ADT.LITR.ZS"    = "Literacy"    , # Literacy rate (% of people ages 15+)
      "SP.URB.TOTL.IN.ZS" = "Urban"         # Urban population (% of total population)
    )

  # fetch data online from World Bank Database
  wdi <<-
    wb(indicator = names(wb_ids), startdate = 2005, enddate = 2018)  %>%
    mutate(var = wb_ids[indicatorID])                                %>%
    select(Country = country, Code = iso3c, Year = date, var, value) %>%
    pivot_wider(names_from = "var", values_from = "value")           %>%
    mutate(Year = as.integer(Year))                                  %>%
    arrange(Code, Year)

  # save a local cache for next time fast-reading
  write_csv(wdi, "../data/wdi.csv")

}





# ================ Load IMF World Economic Outlook ====================


load_weo <- function(year, first = T, second = F) {

  # a trick to accept decimal number as {year.month}
  if (!is.integer(year)) {
    if (round(year + .1) != year) {
      first = F
      second = T
    }
    year <- as.integer(year)
  }

  # IMF World Economic Outlook is a twice-yearly survey
  # the first report is in April, the second in October
  name <- sprintf("WEO%s%dall.xls", ifelse(!first|second, "Oct", "Apr"), year)
  cache <- str_c("../temp/",  name)

  if (!file.exists(cache)) {

    # download the database from IMF website
    url <- sprintf(
      "https://www.imf.org/external/pubs/ft/weo/%d/%s/weodata/%s",
      year,
      ifelse(!first|second, "02", "01"),
      name
    )
    download.file(url, cache, quiet = T, method = "curl")

    if (!file.exists(cache))
      stop(str_c("Failed to download ", url))
  }

  # the downloaded file is actually TSV file (not XLS)
  ds <- read_tsv(cache, na = c("", "n/a", "--"))

  ds <- ds %>%

    # we only need GDP per capital (current USD)
    filter(`WEO Subject Code` == "NGDPDPC") %>%
    select(Code    = ISO,
           After   = "Estimates Start After",
           matches("\\d{4}"))               %>%

    # rearrange the dataframe to long format
    pivot_longer(cols = matches("\\d{4}"),
                 names_to  = "Year",
                 values_to = "Est")         %>%

    mutate(Year = as.integer(Year))         %>%
    filter(Year > After)                    %>%
    select(-After)                          %>%
    arrange(Code, Year)


  return(ds)

}

if (file.exists("../data/weo.csv")) {

  weo <<- read_csv("../data/weo.csv", col_types = "cin")

} else {

  # load a specific report
  weo_2013 <- load_weo(year = 2013) %>% filter(Year > 2011)
  write_csv(weo_2013, "../data/weo.csv")

  # Load WEO from 2006 to 2019, .5 indicates half a year
  # update the estimates with the latest forecast
  weo <-
    seq(2006, 2019 , .5) %>%
    map(possibly(~load_weo(.x), otherwise = NULL)) %>%
    reduce(function(last, this) {
      if (is.null(last))
        return(this)
      else if (is.null(this))
        return(last)
      else {
        # merge the two estimates
        merge(last, this, by = c("Code", "Year"), all.x = T, all.y = T) %>%
          # if set EST to newer estimate  if available
          # or keep it the same as the last estimate
          mutate(Est = ifelse(is.na(Est.y), Est.x, Est.y)) %>%
          select(-Est.x, -Est.y)
      }
    })

  # save a local cache
  write_csv(weo, "../data/weo.csv")

}




# ================== Propensity score matching ====================


# construct the sample for matching
# the matching is based on Year 2012
# TODO: exclude Latin America countries
unmatched <-
  wdi %>% group_by(Code) %>%
  # calculate 5-year average GDP growth rate
  mutate(
    GDP_growth_avg =
      imap_dbl(Year, ~mean(tail(GDP_growth[1:.y], 5)))
  ) %>%
  ungroup(Code) %>%
  filter(Year == 2012) %>%
  select(Code, Pop, KGDP_pc, GDP_growth_avg, Ind_share) %>%
  left_join(country %>% select(Code, Region1), by = "Code") %>%
  na.omit()


# Treatment: the treated group consists of the countries that
# have received investment shock due to the BRI program
treat <-
  invest %>%
  group_by(Code) %>%
  summarise(
    Treatment = any(BRI_shock),
    # record the first year that a shock happened
    Treated_year = first(Year[BRI_shock])
  ) %>%
  filter(Treatment == T)


unmatched$Treatment <- unmatched$Code %in% treat$Code

# run the propensity score matching algorithm
# `optimal` matching finds the matched samples with the smallest
# average absolute distance across all the matched pairs.
match_it <-
  matchit(Treatment ~ log(Pop) + log(KGDP_pc) + GDP_growth_avg + Ind_share,
          data = unmatched, method = "nearest")

matched <- match.data(match_it)
control <- matched %>% select(Code, Treatment) %>% filter(Treatment == F)

# output the result of matching to file
library(stargazer)

match_sum <- summary(match_it)

stargazer(match_sum$sum.all,
          type = "text",
          summary = F,
          out = str_c("../table/sum.all", c(".txt", ".tex")))

stargazer(match_sum$sum.matched,
          type = "text",
          summary = F,
          out = str_c("../table/sum.matched", c(".txt", ".tex")))


# output the countries in the sample
# excluding high income countries
sample <-
  left_join(matched, treat, by = "Code") %>%
  left_join(country, by = "Code") %>%
  filter(Income_group != "High income") %>%
  mutate(Region = str_replace(Region, "\\&", "and")) %>%
  mutate(Group = if_else(Treatment.x, "Treatment", "Control")) %>%
  select(`Country Name` = Country,
         `ISO3 Code` = Code,
         `WB Region` = Region,
         `WB Income Group` = Income_group,
         `Group` = Group
         ) %>%
  arrange(desc(Group), `Country Name`)

stargazer(sample,
          type = "text",
          summary = F,
          digit.separate = 0,
          out = str_c("../table/sample", c(".txt", ".tex")))


# ================ Merge all relevent tables ====================


merged <-
  left_join(wdi, weo, by = c("Code", "Year"))                %>%
  # GDP shock is the diff. btw. actual GDP and IMF Est.
  mutate(GDP_shock = log(GDP_pc) - log(Est))                 %>%

  # keep only obs in matched sample
  filter(Code %in% matched$Code)                             %>%

  left_join(country %>% select(-Country), by = "Code")       %>%
  left_join(bri %>% select(-Country), by = "Code")           %>%
  left_join(invest, by = c("Code", "Year"))                  %>%
  left_join(treat, by = "Code")                              %>%

  # Post_treat if TRUE for years after the first treatment
  mutate(Post_treat = (Year >= Treated_year))                %>%
  # Years after the first treatment (could be negative)
  mutate(Year_shift = Year - Treated_year)                   %>%

  replace_na(list(Invt_count = 0, Invt_quant = 0,
                  Invt_nBRI  = 0, BRI_shock  = F,
                  Treatment  = F, Post_treat = F))           %>%

  arrange(Code, Year)


saveRDS(merged, "../data/merged.rds")






# ======================= END OF SCRIPT ==========================
