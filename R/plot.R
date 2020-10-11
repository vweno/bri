#' =============================================================
#'
#' PROJECT:
#'     Belt & Road Initiative Assesment
#'
#' SCRIPT PURPOSE:
#'     Plotting related scripts.
#'
#' AUTHOR: Z.
#'
#' DATE: 10 MAR 2020
#'
#' NOTICE:  All information contained herein is, and remains
#' the property of the author. No dissemination or reproduction
#' of this information is permitted prior permission is obtained
#' from the author.
#'
#' =============================================================

library(scales)
library(tidyverse)
library(rstudioapi)
library(RColorBrewer)



# global theme setting
theme_set(theme_classic())

# load datasets
setwd(dirname(getActiveDocumentContext()$path))



# ======================== Load datasets  ============================


cgi    <- read_csv("../data/cgi.csv"    , col_types = "ifcnncffccfi")
ctry   <- read_csv("../data/country.csv", col_types = "ffffccfiiiiii")
merged <- readRDS ("../data/merged.rds")



# ================= aggregate investment by year  =====================


cgi %>%
  ggplot(aes(x = Year, y = `Quantity in Millions`, fill = factor(BRI))) +
  stat_summary(fun = sum, geom = 'bar', position = 'stack')             +
  scale_x_continuous(breaks = seq(2005, 2019, 1), expand = c(0, 0.5))   +
  scale_y_continuous(limits = c(0, 100000), expand = c(0, 0))           +
  scale_fill_manual(name     = NULL,
                    labels   = c("Non-BRI countries", "BRI countries", "Others" ),
                    values   = brewer.pal(2, "Set1"),
                    na.value = "grey" )                                 +
  labs(x = NULL, y = "Construction Contracts ($ millions)")             +
  theme(plot.caption.position = "plot",
        plot.caption          = element_text(hjust = 0),
        legend.position       = "bottom")


# ================ aggregate investment by region  ===================


cgi %>%
  filter(Region != "Australia", Region != "USA")                      %>%
  ggplot(aes(x = Year, y = `Quantity in Millions`, color = Region))   +
  stat_summary(fun = sum, geom = 'smooth')                            +
  scale_x_continuous(breaks = seq(2005, 2019, 2), expand = c(0, 0.5)) +
  scale_color_manual(name = NULL,
                     labels = c("Middle East", "East Asia",
                                "Europe", "North America",
                                "South America",
                                "Sub-Saharan Africa",
                                "West Asia"),
                      values = brewer.pal(7, "RdYlBu"))               +
  labs(x = NULL, y = "Construction Contracts ($ millions)")           +
  theme(plot.caption.position = "plot",
        plot.caption          = element_text(hjust = 0),
        legend.position       = "bottom")


# =================== investment by sector and region  ========================


cgi %>%
  select(-Region)                                                           %>%
  left_join(ctry, by = "Code")                                              %>%
  filter(Region != "North America")                                         %>%
  ggplot(aes(x = Year, y = `Quantity in Millions`, fill = Sector))          +
  stat_summary(fun = sum, geom = 'bar', position = 'stack')                 +
  scale_fill_manual(name   = NULL,
                    values = colorRampPalette(brewer.pal(8, "RdYlBu"))(14)) +
  facet_wrap( ~ Region)                                                     +
  theme(panel.spacing.x = unit(.5, "cm"))                                   +
  labs(x = NULL, y = "Construction Contracts ($ millions)")                 +
  theme(strip.background      = element_blank(),
        plot.caption.position = "plot",
        plot.caption          = element_text(hjust = 0),
        legend.position       = "bottom")


# =================== Investment BRI vs non_BRI  ======================


merged %>%
  group_by(Code) %>%
  mutate(BRI_group = if_else((Year < 2013),
                             (BRI_init == 1),
                             (Invt_nBRI > 0)))                     %>%
  ggplot(aes(x = Year,y = Invt_quant / 1000,linetype = BRI_group)) +
  stat_summary(fun = "sum", geom = "line", size = 1)               +
  labs(x = NULL, y = "Construction contracts (Billion USD)")       +
  scale_linetype_manual(name   = NULL,
                        labels = c("Others", "BRI Countries"),
                        values = c("dotdash", "solid"),
                        guide  = guide_legend(reverse = T))        +
  theme_bw() + theme(legend.position   = c(0.15, 0.9),
                     legend.background = element_blank())




# =================== Real GDP (Treatment vs control) ====================


merged %>%
  filter(Year > 2006, Income_group != "High income")                   %>%
  ggplot(aes(x = Year,
             y = log10(KGDP_pc),
             linetype = Treatment,
             group = Treatment))                                       +
  stat_summary(fun = "mean", geom = "line", size = 1)                  +
  scale_linetype_manual(name   = NULL,
                        labels = c("Control", "Treatment"),
                        values = c("dotdash", "solid"),
                        guide  = guide_legend(reverse = T))            +
  labs(x = NULL, y = "Real GDP per capita (constant 2010 US$, Log10)") +
  theme_bw() + theme(legend.position   = c(0.15, 0.9),
                     legend.background = element_blank())



# =========== GDP Growth (Treatment vs control) ===============


merged %>%
  filter(Year > 2006, Income_group != "High income")        %>%
  ggplot(aes(x        = Year,
             y        = GDP_growth,
             linetype = Treatment,
             group    = Treatment ))                        +
  stat_summary(fun = "mean", geom = "line", size = 1)       +
  scale_linetype_manual(name   = NULL,
                        labels = c("Control", "Treatment"),
                        values = c("dotdash", "solid"),
                        guide  = guide_legend(reverse = T)) +
  labs(x = NULL, y = "Average Real GDP Growth")             +
  theme_bw() + theme(legend.position = c(0.85, 0.9),
                     legend.background = element_blank())



# ============ GDP Index (Treatment vs control) ================


merged %>%
  filter(Year > 2006, Income_group != "High income")        %>%
  group_by(Treatment, Year)                                 %>%
  summarize(Value = mean(KGDP, na.rm = T))                  %>%
  group_by(Treatment)                                       %>%
  mutate(Index = Value / first(Value))                      %>%
  ggplot(aes(x = Year, y = Index))                          +
  geom_line(aes(linetype = Treatment), size = 1)            +
  labs(x = NULL, y = "Real GDP Index")                      +
  scale_linetype_manual(name   = NULL,
                        labels = c("Control", "Treatment"),
                        values = c("dotdash", "solid"),
                        guide  = guide_legend(reverse = T)) +
  theme_bw() + theme(legend.position   = c(0.15, 0.9),
                     legend.background = element_blank())




# compare different criteria to define BRI and non-BRI group
merged %>%
  filter(Income_group != "High income", Code != 'CHN')       %>%
  select(Code, Year, KGDP, BRI_init, BRI_core, BRI_latest)   %>%
  pivot_longer (BRI_init:BRI_latest,
               names_to  = "Group",
               values_to = "Belongs")                        %>%
  unite("Group", Belongs, Group)                             %>%
  group_by(Year, Group)                                      %>%
  summarize(LogGDP = mean(log(KGDP), na.rm = T))             %>%
  group_by(Group)                                            %>%
  mutate(GrowthIndex = LogGDP - first(LogGDP))               %>%
  ggplot(aes(x = Year, y = GrowthIndex, col = Group))        +
  stat_summary(fun = "mean", geom = "line")                  +

  scale_x_continuous(breaks = seq(2008, 2018, 2))            +
  scale_color_manual(name = NULL,
                     labels = c("Non-Core BRI countries",
                                "Non-Initial BRI countries",
                                "Non-BRI countries",
                                "Core BRI countries",
                                "Initial BRI countries",
                                "Latest BRI countries"),
                     values = brewer.pal(6, "RdYlBu"))       +
  labs(x = NULL, y = "GDP per capita (cumulative growth)")




# ================ GDP Trend by Region =====================


merged %>%
  filter(Income_group != "High income", Code != "CHN")    %>%
  group_by(Code) %>% mutate(GDP_static = last(GDP_pc))    %>%
  ggplot(aes(x = Year, y = GDP_pc, col = Treatment))      +
  geom_line(aes(group = Code, alpha = log10(GDP_static))) +
  geom_text(aes(x = 2019, y = GDP_static, label = Code),
            alpha = .1)                                   +
  facet_wrap( ~ Region)                                   +
  scale_y_continuous(limits = c(0, 15000))                +
  scale_x_continuous(breaks = seq(2007, 2018, 3))         +
  scale_alpha_continuous(name = "LOG(GDP)")               +
  scale_color_manual(name = "B&R",
                     labels = c("NO", "YES"),
                     values = c("#74ADD1", "#F46D43"))    +
  labs(x = NULL, y = "GDP per capita (constant 2010 US$)")+
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"))




# ================ GDP per capita Distribution =====================


merged %>%
  filter(Income_group != 'High income')                      %>%
  filter(Year %in% 2009:2017)                                %>%
  mutate(Year_BRI = str_c(Year, Treatment, sep = "_"))       %>%
  ggplot(aes(x = log(GDP_pc), color = Year_BRI))             +
  geom_density()                                             +
  scale_x_continuous(expand = c(0,0))                        +
  scale_y_continuous(expand = c(0,0))                        +
  scale_color_manual(values = rbind(brewer.pal(9, "Blues"),
                                    brewer.pal(9, "Reds")))  +
  labs(x = "GDP per capita (constant 2010 US$, Log scale)",
       y = "Density")                                        +
  theme(legend.position = "none",
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))



# ================== BRI Investment / GDP =====================


merged %>%
  mutate(Ratio = Invt_quant * 10^6 / GDP)                                 %>%
  filter (Treatment == T, Year > 2012)                                    %>%
  select(Code,Year, GDP, Ratio)                                           %>%
  ggplot(aes(x = GDP, y = Ratio))                                         +
  geom_point(position = "jitter")                                         +
  scale_y_log10(breaks = c(.001, .01, .1, .025))                          +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format()))            +
  geom_hline(aes(yintercept = mean(Ratio, na.rm = T)), linetype="dashed") +
  labs(x = "GDP (current USD)", y = "Annual BRI Investment / GDP")        +
  theme_bw()





# ======================= END OF SCRIPT  ==========================
