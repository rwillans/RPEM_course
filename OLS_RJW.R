#OLS_RJW

library(tidyverse)
library(MatchIt)
library(cobalt)
library(AER)
library(estimatr)
library(linear)
source("film.R")

#Load in nsw_psid

nsw_psid <- read_csv("nsw_psid.csv")

#1 Homogenous impacts and dummy variable model

#a1 treatment effect -7,423

lm(re78 ~ treated + age + age2 + black + hispanic + married, data = nsw_psid) |> summary()

#a2 treatment effect -12,697

lm(re78 ~ treated + educ + nodegree, data = nsw_psid) |> summary()

#a3 treatment effect -2,380

lm(re78 ~ treated + re75, data = nsw_psid) |> summary()

#a3 treatment effect -1,348

lm(re78 ~ ., data = nsw_psid) |> summary()

#b estimates depend on other controlls

#c not very well

#d all of them to a degree, particularly previous earnings, education, married and hispanic

#e different models

lm(re78 ~ treated + treated*educ + re75 + educ + married + hispanic, data = nsw_psid) |> summary()
lm(re78 ~ treated + treated*educ + treated*re75 + re75 + educ + married + hispanic, data = nsw_psid) |> summary()
lm(re78 ~ treated + treated*educ + treated*re75 + re75 + educ + married + hispanic, data = nsw_psid) |> summary()

#seems like indications that effect decreases with prior income increases

#f 

full_model <- lm(re78 ~ treated + treated*educ + treated*re75 + re75 + educ + married + hispanic, data = nsw_psid)

linearHypothesis(full_model, c("treated:educ = 0", "treated:re75 = 0"), test = "F")

#g - ATT is -1033 ??
film(nsw_psid, "re78", "treated", c('educ', 're75', 'married', 'hispanic'), robust = TRUE)
