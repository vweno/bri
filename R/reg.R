#' =============================================================
#'
#' PROJECT:
#'     Belt & Road Initiative Assesment
#'
#' SCRIPT PURPOSE:
#'     Hanlde all regressions.
#'
#' AUTHOR: Z.
#'
#' DATE: 12 APRIL 2020
#'
#' NOTICE:  All information contained herein is, and remains
#' the property of the author. No dissemination or reproduction
#' of this information is permitted prior permission is obtained
#' from the author.
#'
#' =============================================================

library(rstudioapi)
library(stargazer)
library(tidyverse)
library(latex2exp)
library(sandwich)
library(lmtest)
library(broom)


# ======================= Utility Functions ========================

#' An internal function used to wrap up regression result to be
#' passed into `sumtable` for display and export.
#'
#' @param model the regression model
#' @param robust types of robust standard error: HC1, HC2, HC3, etc.
#' @param cluster the column name of the cluster variable
#' @param treatment a vector of variables to be displayed in the table
#' @param controls a list of controls to be displayed (see @example)
#'
#' @return a `.sum` object that can be passed to `sumtable`
#'
#' @examples
#' .sum(model, controls = list(`Year FE` = TRUE, `Country FE` = TRUE))
#'

.sum <- function(model,
                 name      = NULL,
                 robust    = "HC1",
                 cluster   = NULL,
                 treatment = NULL,
                 controls  = NULL ) {

  require(sandwich)

  if (is.null(treatment)) stop("Treatment must be specified!")

  se <- NULL # robust s.e. to be returned

  if (!is.null(cluster)) {

    if (is.character(cluster)) {
      # evaluate the cluster variable in the model environment
      call <- getCall(model)
      data <- eval(call$data, envir = environment(formula(model)))
      cluster <- data[[cluster]]
    }

    vcov <- sandwich::vcovCL(model, cluster = cluster, type = robust)
    se <- sqrt(diag(vcov))

  } else {

    vcov <- sandwich::vcovHC(model, type = robust)
    se <- sqrt(diag(vcov))

  }

  s <- structure(list(
    name      = name,
    model     = model,
    robust.se = se,
    controls  = controls,
    treatment = treatment
  ),
  class = ".sum")

  return(s)
}


#' A wrapper on `stargazer` to format regression result to
#' publication quality tables.
#'
#' @param ... `.sum` wrapped regression models
#' @param output a vector that contains the path(s) of output file
#'
#' @return the output of `stargazer` will be printed
#' @export

sumtable <- function(..., output = NULL) {

  require(stargazer)

  sums <- list(...)

  for (each in sums) {
    if (class(each) != ".sum")
      stop("Invalid argument. Models must be wrapped with .sum function.")
  }

  mods <- map(sums, ~.x$model)

  # arguments to be passed to stargazer
  args <- list(
    type = "text",
    out = output,
    align = F,
    out.header = FALSE,
    dep.var.caption = "",
    dep.var.labels.include = FALSE,
    se = map(sums, ~.x$robust.se),
    keep = map(sums, ~.x$treatment) %>% reduce(~c(.x, .y)),
    keep.stat = c("n", "adj.rsq")
  )

  # combine all fixed effects, using names as the first row
  fe <- (function(controls) {
    t <-
      bind_rows(controls) %>%
      mutate_all(as.character) %>%
      mutate_all(~str_replace(., "TRUE", "Yes")) %>%
      mutate_all(~replace_na(., "No"))
    n <- names(t)
    names(n) <- names(t)
    t <- bind_rows(n, t)
    return(t)
  }) (map(sums, ~.x$controls))

  if (length(fe) > 0)  args$add.lines <- as.list(fe)


  # naming colume labels if any name is given
  names <- map_chr(sums, ~.x$name %||% NA)

  if (any(!is.na(names)))  args$column.labels <- names


  do.call(stargazer::stargazer, args = c(mods, args))

}



# ======================= Load Data =============================

# set working directory to source file location
setwd(dirname(getActiveDocumentContext()$path))


# Load data keeping only developing countries and years after GFC
sample <-
  readRDS("../data/merged.rds") %>%
  filter(Year > 2008, Income_group != "High income")


# create some categorical variables for notational convenience
sample$i_Code <- factor(sample$Code)
sample$i_Year <- factor(sample$Year)
sample$i_Time <- factor(sample$Year_shift)

result <- list() # collection of regression results



# in debug mode, halt execution to avoid overriding existing results
if ((debug_mode = F)) stop("DATA LOADED! STOP EXECUTION!", call. = F)


# ========================= Main specification ===========================


result$m1 <- lm(log(KGDP) ~ Post_treat + i_Code + i_Year, data = sample)

result$m2 <- lm(log(KGDP) ~ Post_treat + i_Code + i_Year +
                            LDC:i_Year + Landlocked:i_Year, data = sample)

result$m3 <- lm(log(KGDP) ~ Post_treat  + i_Code + i_Year   +
                            LDC:i_Year  + Landlocked:i_Year +
                            Fuel:i_Year + BRI_init:i_Year, data = sample)

result$m4 <- lm(log(KGDP) ~ Post_treat  + i_Code + i_Year   +
                            LDC:i_Year  + Landlocked:i_Year +
                            Fuel:i_Year + BRI_init:i_Year   +
                            i_Code:Year , data = sample)

sumtable(.sum(result$m1, treatment = "Post_treat",
              controls = list(`Country FE` = T, `Year FE` = T)),

         .sum(result$m2, treatment = "Post_treat",
              controls = list(`Country FE` = T, `Year FE`         = T,
                              `LDC * FE`   = T, `Landlocked * FE` = T)),

         .sum(result$m3, treatment = "Post_treat",
              controls = list(`Country FE` = T, `Year FE`         = T,
                              `LDC * FE`   = T, `Landlocked * FE` = T,
                              `Fuel * FE`  = T, `Belt&Road * FE`  = T)),

         .sum(result$m4, treatment = "Post_treat",
              controls = list(`Country FE` = T, `Year FE`         = T,
                              `LDC * FE`   = T, `Landlocked * FE` = T,
                              `Fuel * FE`  = T, `Belt&Road * FE`  = T,
                              `Country * Trend` = T)),

         output = str_c("../table/table1", c(".txt", ".tex")))



# Estimate treatment effect by region and income group

result$m5 <- lm(log(KGDP) ~ Post_treat + i_Code + i_Year +
                            as.integer(Post_treat):Region,
                            data = sample)

result$m6 <- lm(log(KGDP) ~ Post_treat + i_Code + i_Year +
                            as.integer(Post_treat):Income_group,
                            data = sample)


sumtable(.sum(result$m5, treatment = "Post_treat",
              controls = list(`Country FE` = T, `Year FE` = T)),

         .sum(result$m6, treatment = "Post_treat",
              controls = list(`Country FE` = T, `Year FE` = T)),

         output = str_c("../table/table1a", c(".txt", ".tex")))



# controlling more time varying factors

result$r1 <- lm(log(KGDP) ~ Post_treat + i_Code + i_Year +
                            Urban, data = sample)

result$r2 <- lm(log(KGDP) ~ Post_treat + i_Code + i_Year +
                            Urban      + School, data = sample)

result$r3 <- lm(log(KGDP) ~ Post_treat + i_Code + i_Year +
                            Urban      + School + log(Aid), data = sample)

result$r4 <- lm(log(KGDP) ~ Post_treat + i_Code + i_Year +
                            Urban      + School + log(Aid) +
                            log(KGDP*Exp_share), data = sample)


sumtable(.sum(result$r1, treatment = c("Post_treat", "Urban"),
              controls = list(`Country FE` = T, `Year FE` = T)),

         .sum(result$r2, treatment = c("Post_treat", "Urban", "School"),
              controls = list(`Country FE` = T, `Year FE` = T)),

         .sum(result$r3, treatment = c("Post_treat", "Urban", "School", "Aid"),
              controls = list(`Country FE` = T, `Year FE` = T)),

         .sum(result$r4, treatment = c("Post_treat", "Urban", "School", "Aid", "Exp"),
              controls = list(`Country FE` = T, `Year FE` = T)),

         output = str_c("../table/table2", c(".txt", ".tex")))



# ================== Robustness Check: Timing ======================

# Timing evidence is crucial to a causal interpretation of these results.
# Test whether the economic growth postdated the BRI investment.

# coding dummies for whether a country will be treated in 3-5 years.
sample1 <-
  sample %>%
  group_by(Code) %>%
  mutate(
    Prior_treatment_2 = (Year + 2 == Treated_year),
    Prior_treatment_3 = (Year + 3 == Treated_year),
    Prior_treatment_4 = (Year + 4 == Treated_year),
    Prior_treatment_5 = (Year + 5 == Treated_year)
  ) %>%
  replace_na(
    list(Lead_BRI_2 = F, Lead_BRI_3 = F, Lead_BRI_4 = F, Lead_BRI_5 = F)
  )

result$t2 <- lm(log(KGDP) ~ Prior_treatment_2 + i_Code + i_Year, data = sample1)
result$t3 <- lm(log(KGDP) ~ Prior_treatment_3 + i_Code + i_Year, data = sample1)
result$t4 <- lm(log(KGDP) ~ Prior_treatment_4 + i_Code + i_Year, data = sample1)
result$t5 <- lm(log(KGDP) ~ Prior_treatment_5 + i_Code + i_Year, data = sample1)

sumtable(.sum(result$t2, treatment = "treatment", controls = list(`Country FE` = T, `Year FE` = T)),
         .sum(result$t3, treatment = "treatment", controls = list(`Country FE` = T, `Year FE` = T)),
         .sum(result$t4, treatment = "treatment", controls = list(`Country FE` = T, `Year FE` = T)),
         .sum(result$t5, treatment = "treatment", controls = list(`Country FE` = T, `Year FE` = T)),
         output = str_c("../table/table3", c(".txt", ".tex")))


# Dynamic Effect
# i_Time is a dummy that is TRUE if it has been i years after the first treatment
# note this exludes all countries in the control group
result$td <- lm(log(KGDP) ~ i_Time      + i_Code:Year       +
                            LDC:i_Year  + Landlocked:i_Year +
                            Fuel:i_Year + BRI_init:i_Year   +
                            i_Code      + i_Year, data = sample)


result$td.sum <- .sum(result$td,
                      treatment = str_c("i_Time", -5:5),
                      controls = list(`All FE` = T))

sumtable(result$td.sum, output = str_c("../table/table4", c(".txt", ".tex")))


# plot the dynamic coefficient
tibble(term      = -5:5,
       est       = result$td.sum$model$coefficients[str_c("i_Time", -5:5)],
       se        = result$td.sum$robust.se[str_c("i_Time", -5:5)],
       conf.low  = est - 1.96 * se,
       conf.high = est + 1.96 * se) %>%
  ggplot(aes(term, est)) + geom_point()                   +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  scale_x_continuous(breaks = -5:5)                       +
  labs(x = "Years After Treatment", y = "Coefficients")   +
  geom_hline(yintercept = 0)                              +
  theme_bw()



# ================== Robustness Check: Selection ======================


# GDP_shock = Actual Grwoth - IMF Forecasted Growth
# IMF forecast takes into account various factors that could influence
# growth potential.
# The difference reflects the impact from factors that IMF failed to
# take into account.
result$s1 <- lm(GDP_shock ~ Post_treat + i_Code + i_Year, data = sample)

result$s2 <- lm(GDP_shock ~ Post_treat + i_Code + i_Year +
                            LDC:i_Year + Landlocked:i_Year,
                            data = sample)

result$s3 <- lm(GDP_shock ~ Post_treat  + i_Code + i_Year   +
                            LDC:i_Year  + Landlocked:i_Year +
                            Fuel:i_Year + BRI_init:i_Year,
                            data = sample)

sumtable(.sum(result$s1, name = "Shock", treatment = "Post_treat",
              controls = list(`Country FE` = T, `Year FE` = T)),

         .sum(result$s2, name = "Shock", treatment = "Post_treat",
              controls = list(`Country FE` = T, `Year FE`         = T,
                              `LDC * FE`   = T, `Landlocked * FE` = T)),

         .sum(result$s3, name = "Shock", treatment = "Post_treat",
              controls = list(`Country FE` = T, `Year FE`         = T,
                              `LDC * FE`   = T, `Landlocked * FE` = T,
                              `Fuel * FE`  = T, `Belt&Road * FE`  = T)),

         output = str_c("../table/table5", c(".txt", ".tex")))



# ================== Robustness Check: Sensitivity Test ======================



# SST1: randomly drop 10 countries from the sample
sst1 <- map(1:100, possibly(function(.) {

  c  <- sample(unique(sample$Code), length(unique(sample$Code)) - 10)
  s  <- sample %>% filter(Code %in% c)
  m  <- lm(log(KGDP) ~ Post_treat + i_Code + i_Year, data = s)
  se <- sqrt(diag(sandwich::vcovHC(m, type = "HC1")))

  list(coef = m$coefficients["Post_treatTRUE"], se = se["Post_treatTRUE"])

}, otherwise = NULL))

# plot the distribution of the estimated coefficients
tibble(
  coef     = map_dbl(sst1, ~ .x$coef),
  se       = map_dbl(sst1, ~ .x$se),
  conf.min = coef - 1.96 * se,
  conf.max = coef + 1.96 * se
  ) %>%
  pivot_longer(cols      = starts_with("co"),
               names_to  = "var",
               values_to = "value") %>%
  ggplot(aes(x = value, color = factor(var))) + geom_density()    +
  scale_color_manual(values = c("#000000", "#AAAAAA", "#BBBBBB")) +
  labs(x = "Treatment Effect", y = "Density") +
  theme_bw() + theme(legend.position = "none")



# prepare to test sensitivity for different criteria of
# treatment identification
invest  <- read_csv("../data/invest.csv", col_types = "cinnnl") %>%
           select(-BRI_shock)
sample2 <- readRDS("../data/merged.rds")                        %>%
           filter(Year > 2008, Income_group != "High income")   %>%
           select(Code, Year, KGDP)


# a function to identify treatment by different thresholds
# level: the minimum level of investmnet required
# scalar: post-BRI investment has to be this times greater
#         than previous investments
.run <- function(level, scaler) {

  fun <- function(c, i, p, q) {
    if (p[[i]] > 0 & q[[i]] > level) {
      if (i == 1) TRUE
      else if (q[[i]] > scaler * max(tail(q[1:(i - 1)], 3))) TRUE
      else NA
    } else NA
  }

  tbl <-
    invest %>% group_by(Code)                                      %>%
    mutate(Treatment = imap_lgl(Code, fun, Invt_nBRI, Invt_quant)) %>%
    right_join(sample2, by = c("Code", "Year"))                    %>%
    group_by(Code) %>% fill(Treatment, .direction = "down")        %>%
    replace_na(list(Treatment = F))

  m <- lm(log(KGDP) ~ Treatment + factor(Code) + factor(Year), data = tbl)
  se <- sqrt(diag(sandwich::vcovHC(m, type = "HC1")))

  list(coef = m$coefficients["TreatmentTRUE"], se = se["TreatmentTRUE"])

}

# SST2: test how much greater a BRI investment has to be
# compared to previous investmnet
sst2 <- map(seq(1, 3, .1), ~ .run(level = 800, scaler = .x))

# plot the distribution of the estimated coefficients
tibble(
  coef     = map_dbl(sst2, ~ .x$coef),
  se       = map_dbl(sst2, ~ .x$se),
  conf.min = coef - 1.96 * se,
  conf.max = coef + 1.96 * se
  ) %>%
  ggplot(aes(x=seq(1, 3, .1), y=coef)) + geom_point()    +
  geom_pointrange(aes(ymin = conf.min, ymax = conf.max)) +
  labs(x = TeX("$\\kappa$"), y = "Treatment Effect")     +
  geom_hline(yintercept = 0)                             +
  theme_bw()


# SST3: test the minimal threshold of quantity of investment
# required to be identified as treatment
sst3 <- map(seq(100, 2000, 100), ~ .run(level = .x, scaler = 2))

# plot the distribution of the estimated coefficients
tibble(
  coef     = map_dbl(sst3, ~ .x$coef),
  se       = map_dbl(sst3, ~ .x$se),
  conf.min = coef - 1.96 * se,
  conf.max = coef + 1.96 * se
  ) %>%
  ggplot(aes(x = seq(100, 2000, 100), y = coef)) + geom_point() +
  geom_pointrange(aes(ymin = conf.min, ymax = conf.max))        +
  labs(x = TeX("$\\theta$"), y = "Treatment Effect")            +
  geom_hline(yintercept = 0)                                    +
  theme_bw()



# ================== Other variables of interest ======================



# Check the impact on agriculture, industry and trade

result$a1 <- lm(log(KGDP*Agr_share) ~ Post_treat  + i_Code + i_Year   +
                                      LDC:i_Year  + Landlocked:i_Year +
                                      Fuel:i_Year + BRI_init:i_Year,
                                      data = sample)

result$a2 <- lm(log(KGDP*Ind_share) ~ Post_treat  + i_Code + i_Year   +
                                      LDC:i_Year  + Landlocked:i_Year +
                                      Fuel:i_Year + BRI_init:i_Year,
                                      data = sample)

result$a3 <- lm(log(KGDP*Trd_share) ~ Post_treat  + i_Code + i_Year   +
                                      LDC:i_Year  + Landlocked:i_Year +
                                      Fuel:i_Year + BRI_init:i_Year,
                                      data = sample)


sumtable(.sum(result$a1,
              name = "Agr. VA",
              treatment = "Post_treat",
              controls = list(`All FE` = T)),

         .sum(result$a2,
              name = "Ind. VA",
              treatment = "Post_treat",
              controls = list(`All FE` = T)),

         .sum(result$a3,
              name = "Trade",
              treatment = "Post_treat",
              controls = list(`All FE` = T)),

         output = str_c("../table/table6", c(".txt", ".tex")))







# ======================== END OF SCRIPT ==============================
