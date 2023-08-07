library(tidyverse)
library(MatchIt)
library(cobalt)
library(AER)
source('pstest0.R')
source('pstest_scatter.R')
options(scipen = 999)

#Read in nsw_psid

nsw_psid <- read_csv("nsw_psid.csv")

# Take 'married' as the instrument

#1 Wald Estimator - difference between outcomes according to instrument divided (scaled) by
# increased probability of treatment with instrument

wald_numerator <- mean(nsw_psid$re78[nsw_psid$married==1]) - mean(nsw_psid$re78[nsw_psid$married==0])
wald_denominator <- mean(nsw_psid$treated[nsw_psid$married==1]) - mean(nsw_psid$treated[nsw_psid$married==0])

wald_numerator / wald_denominator

# Estimated effect is -25,333.5                         

ivreg(re78 ~ treated | married, data = nsw_psid)

# gets same result of -25,333

#2 2SLS

#c lets go with educ, hispanic, and re75 (married as instrument)

#stage1 regress treatment on instrument and covariates

stage1 <- lm(treated ~ married + educ + hispanic + re75, data = nsw_psid)

nsw_psid <- nsw_psid |> mutate(t_hat = predict(stage1))

#stage2 regress outcome on treatment_hat + covariates

stage2 <- lm(re78 ~ t_hat + educ + hispanic + re75, data = nsw_psid)
summary(stage2)

# effect is -5,151.74?

#e check with IVreg - effect is -5,151.74!

ivreg(re78 ~ treated + educ + hispanic + re75 | married + educ + hispanic + re75, data = nsw_psid)


