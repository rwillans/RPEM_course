## 	Barbara Sianesi
##  (barbarasianesi@gmail.com)
##	POLICY EVALUATION METHODS Course
##	
##	PRACTICAL SESSION 3 
##  INSTRUMENTAL VARIABLES 
##  ************************************************************************

##	Some initial R code kindly contributed by Anas Abu (formerly at DCMS)
## 	Any mistakes remain mine (Barbara)
##	In the spirit of R, if you can improve this script, please let me know :)


# married as instrument

#install.packages("ivreg", dependencies = TRUE)
library(ivreg)
library(AER)
library(jtools)

#setwd("your working directory")

nsw_psid <- read.csv("nsw_psid.csv")


## 1. Wald estimator    						      
## ************************************************************************

m1 <- mean(nsw_psid$re78[nsw_psid$married==1])
m0 <- mean(nsw_psid$re78[nsw_psid$married==0])

p1 <- mean(nsw_psid$treated[nsw_psid$married==1])
p0 <- mean(nsw_psid$treated[nsw_psid$married==0])

(m1 - m0)/(p1 - p0)


iv_wald <- ivreg(re78 ~ treated  |  married, data = nsw_psid )
summary(iv_wald)


## 2. 2SLS estimator    							
## ************************************************************************

# Stage 1 - use the instrument (married) to explain the treatment

stage1 <- lm(treated ~ age + educ + black + hispanic + nodegree + re75 + married, data = nsw_psid)

summ(stage1, digits = 3)

# So instrument(married) reduces liklihood of treatment by 28.5%?

linearHypothesis(stage1, c("married=0"))

# Take predicted values from stage 1 (predicted treatment based on instrument(married))
tr.hat <- fitted.values(stage1)

# Second stage - use predicted values for treatment to predict outcome
# implies treatment reduces earnings by 7,112?

tsls <- lm(re78 ~ tr.hat + age + educ + black + hispanic + nodegree + re75, data = nsw_psid)

summ(tsls, digits = 3)

# in IV_mod - queation is main regression, second is 'first stage' setting out regression
# (outcome is treated)

iv_mod <- ivreg(re78 ~ treated + age + black + hispanic + educ + nodegree + re75 |
              age + black + hispanic + educ + nodegree + re75 + married, data = nsw_psid )

summary(iv_mod)


# Robust std errors

coeftest(iv_mod, vcov = vcovHC, type = "HC1")


# Alternative implementation via control function

v.hat <- residuals(stage1)

tsls_cf <- lm(re78 ~ treated + age + educ + black + hispanic + nodegree + re75 + v.hat, data = nsw_psid)

summ(tsls_cf, digits = 3)


# Also consider
source("pstest0.R")
treated <- "married"
covariates <- c("age", "educ", "black", "hispanic", "nodegree", "re75")
pstest0(varlist = covariates, treated = treated, data = nsw_psid)


