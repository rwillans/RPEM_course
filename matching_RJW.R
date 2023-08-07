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

# read in data

nsw_psid <- read_csv("nsw_psid.csv",
                     col_types = cols(treated = col_integer(), 
                                      age = col_integer(), age2 = col_integer(), 
                                      educ = col_integer(), black = col_integer(), 
                                      hispanic = col_integer(), married = col_integer(), 
                                      nodegree = col_integer()))

#Establish covariates
covs <- c('age', 'educ', 'black', 'hispanic', 'married', 'nodegree', 're75')



# Estimate ATNT for non-treated

#create non-treated var

nsw_psid <- nsw_psid |> 
  mutate(non_treat = case_when(!treated ~ 1, TRUE ~ 0))

#estimate a propensity score as probit

form <- as.formula(paste("non_treat ~", paste(covs, collapse = "+")))

score_mod <- glm(form, data = nsw_psid, family = binomial(link = "probit"))

nsw_psid <- nsw_psid |> mutate(score = score_mod$fitted.values)

#check support

ggplot(nsw_psid, aes(x = as.factor(non_treat), y = score)) + geom_boxplot()

bal.plot(
  x = score_mod,
  data = nsw_psid,
  treat = "non_treat",
  var.name = "nodegree",
  type = "histogram",
  mirror = TRUE
)

# play around with estimators

summary(score_mod, improvement = TRUE)
love.plot(score_mod, binary = "std")
#pstest_scatter(score_mod, xmax = 200)

#ATNT

match <- matchit(form, data = nsw_psid, link = "probit", estimand = "ATC")
m_data <- match.data(match, drop.unmatched = FALSE)

bias_m <- pstestM(varlist = covs, treated = "non_treat", data = m_data)
bias_m

lm(re78 ~ treated, weights = weights, data = m_data)

film(data = nsw_psid, outcome = "re78", treated = "treated", covariates = covs, robust = TRUE, effect = "ATNT")

film(data = nsw2, outcome = "re78", treated = "treated", covariates = covs, robust = TRUE, effect = "ATNT")
