## 	Barbara Sianesi
##  (barbarasianesi@gmail.com)
##	POLICY EVALUATION METHODS Course
##	
##	PRACTICAL SESSION 5
##  ORDINARY LEAST SQUARES  
##  ************************************************************************

##	Some initial R code kindly contributed by Anas Abu (formerly at DCMS)
## 	Any mistakes remain mine (Barbara)
##	In the spirit of R, if you can improve this script, please let me know :)



#install.packages("estimatr")
library(estimatr)
library(tidyverse)
library(AER)
library(jtools)


#setwd("your working directory")

nsw_psid <- read.csv("nsw_psid.csv")

summ(lm(re78 ~ treated + age + age2 + black + hispanic + married, data = nsw_psid), digits = 3)

summ(lm(re78 ~ treated + educ + nodegree, data = nsw_psid), digits = 3)

summ(lm(re78 ~ treated + re75, data = nsw_psid), digits = 3)

summ(lm(re78 ~ treated + age + age2 + black + hispanic + married + educ + nodegree + re75, data = nsw_psid), digits = 3) 

# Note: preferable to use robust standard errors
# (summ does not work on lm_robust)

lm_robust(re78 ~ treated + age + age2 + black + hispanic + married + educ + nodegree + re75, data = nsw_psid)


# Calculating interactions to allow for heterogenous impacts

nsw_psid <- nsw_psid %>%
  mutate(age_D 	= age*treated,
 		 age2_D 	  = age2*treated,
 		 educ_D 	  = educ*treated,
		 black_D 	  = black*treated,
 		 hispanic_D = hispanic*treated,
 		 married_D  = married*treated,
  	 nodegree_D = nodegree*treated,
  	 re75_D 	  = re75*treated)

ols_int <- lm(re78 ~ treated 
        + age_D + age2_D + educ_D + black_D + hispanic_D + married_D + nodegree_D + re75_D
        + age + age2 + black + hispanic + married + educ + nodegree + re75, data = nsw_psid)
summ(ols_int, digits = 3)


# testing the hypothesis that the variables are jointly insignificant 
linearHypothesis(ols_int, c("age_D=0", "age2_D=0", "educ_D=0", "black_D=0", "hispanic_D=0", "married_D=0", "nodegree_D=0", "re75_D=0"), test = "F")

# robust std errors
ols_int_rb <- lm_robust(re78 ~ treated 
              + age_D + age2_D + educ_D + black_D + hispanic_D + married_D + nodegree_D + re75_D
              + age + age2 + black + hispanic + married + educ + nodegree + re75, data = nsw_psid)

linearHypothesis(ols_int_rb, c("age_D=0", "age2_D=0", "educ_D=0", "black_D=0", "hispanic_D=0", "married_D=0", "nodegree_D=0", "re75_D=0"), test = "F")


# Using the film.R script
source("film.R")

covariates <- c("age", "age2", "black", "hispanic", "married", "educ", "nodegree", "re75")

film(data = nsw_psid, outcome = "re78", treated = "treated", 
     covariates = covariates, robust = FALSE)
film(data = nsw_psid, outcome = "re78", treated = "treated", 
     covariates = covariates, robust = TRUE)

cov  <- c("re75")
lcov <- c("age", "age2", "black", "hispanic", "married", "nodegree", "educ")

film(data = nsw_psid, outcome = "re78", treated = "treated", 
     covariates = cov, lin_cov = lcov, robust = FALSE)
film(data = nsw_psid, outcome = "re78", treated = "treated", 
     covariates = cov, lin_cov = lcov, robust = TRUE)

