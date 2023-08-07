library(tidyverse)
library(MatchIt)
library(cobalt)
library(AER)
library(marginaleffects)
source('pstest0.R')
source('pstestM.R')
source('pstest_scatter.R')
source('film.R')
options(scipen = 999)

#load nsw_psid

nsw_psid <- read_csv("nsw_psid.csv",
                     col_types = cols(treated = col_integer(), 
                                      age = col_integer(), age2 = col_integer(), 
                                      educ = col_integer(), black = col_integer(), 
                                      hispanic = col_integer(), married = col_integer(), 
                                      nodegree = col_integer()))

treated <- nsw_psid |> filter(treated == 1)

#Before / After - 5976 effect!
lm(re78 ~ 1, data = treated)

# DiD estimator - close to rct

lm((re78-re75) ~ treated, data = nsw_psid)


# Control for age - effect now -876 + age of participant??

lm((re78-re75) ~ treated + age + age2, data = nsw_psid)

#control for pre-prog earnings - effect is treated + 0.37*previous income

lm((re78-re75) ~ treated + re78, data = nsw_psid)

lm((re78-re75) ~ treated + re78, data = nsw_psid)

#control for X in non-para way ~ 4958.4

matched_data_out <- matchit(treated ~ age + educ + black + hispanic + married + nodegree + (re78-re75), data = nsw_psid, replace = TRUE) |> match.data(drop.unmatched = FALSE)

summary(lm(re78-re75 ~ treated, weights = weights, data = matched_data_out))

summary(matched_data)
