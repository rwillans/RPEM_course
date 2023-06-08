## 	Barbara Sianesi
##  (barbarasianesi@gmail.com)
##	POLICY EVALUATION METHODS Course
##	
##	PRACTICAL SESSION 2 
##  RANDOMISED EXPERIMENTS 
##  ************************************************************************

##	Some initial R code kindly contributed by Anas Abu (formerly at DCMS)
##  Thank you also to Alex Stroud (DCMS)
## 	Any mistakes remain mine (Barbara)
##	In the spirit of R, if you can improve this script, please let me know :)



## 	Get acquainted with the data
##  ************************************************************************

#setwd("your working directory")

nsw <- read.csv("nsw.csv")

str(nsw)

summary(nsw)

summary(nsw$treated)

addmargins(table(nsw$treated))


## 1. Check random assignment
## ************************************************************************

#install.packages("MatchIt")
#install.packages("cobalt")
library(MatchIt)
library(cobalt)

rct_bal <- matchit(treated ~ age + educ + black + hispanic + married + nodegree + re75,
					 data = nsw, method = NULL)
summary(rct_bal)
plot(summary(rct_bal))
love.plot(rct_bal, binary = "std")
bal.plot(rct_bal, var.name = "re75")
plot(rct_bal, type = "density", which.xs = c("age", "educ", "black"))


# Let's try with my pstest for R
# Make sure the scripts are in your working directory
source("pstest0.R")
source("pstest_scatter.R")

treated <- "treated"
covariates <- c("age", "educ", "black", "hispanic", "married", "nodegree", "re75")

pstest0(varlist = covariates, treated = treated, data = nsw)

# Scatter plot
result <- pstest0(varlist = covariates, treated = treated, data = nsw)
# ck out max bias to decide on xmax
result
pstest_scatter(data=result, xmax=200)

library(ggplot2)
ggplot(nsw, aes(x = as.factor(treated), y = re75)) + geom_boxplot()


## 2. Derive the benchmark: Experimental impact estimates 		
## *************************************************************************

rct_mod0 <- lm(re78 ~ treated, data = nsw)
summ(rct_mod0, digits = 3)


## Regression adjustment 		
## *************************************************************************

# LaLonde's model
rct_modL <- lm(re78 ~ treated + age + age2 + educ + black + hispanic + nodegree, data = nsw)
summ(rct_modL, digits = 3)

# Compare beta from
summ(rct_mod0, digits = 3)

rct_mod1 <- lm(re78 ~ treated + age + age2 + educ + black + hispanic + re75, data = nsw)
summ(rct_mod1, digits = 3)

rct_mod2 <- lm(re78 ~ treated + nodegree, data = nsw)
summ(rct_mod2, digits = 3)

# Compare SE(beta) from
summ(rct_mod0, digits = 3)

summ(rct_modL, digits = 3)

rct_mod3 <- lm(re78 ~ treated + re75, data = nsw)
summ(rct_mod3, digits = 3)


## 3. Heterogeneous impacts by age 		
## *************************************************************************

rct_old <- lm(re78 ~ treated, data = nsw, nsw$age > 24)
summ(rct_old, digits = 3)

rct_young <- lm(re78 ~ treated, data = nsw, nsw$age <= 24)
summ(rct_young, digits = 3)


# Formally test the significance of the difference:

library(tidyverse)

nsw <- nsw %>%
  mutate(treated_old = ifelse(nsw$treated==1 & nsw$age > 24, 1, 0))

summ(lm(re78 ~ treated + treated_old, data = nsw), digits = 3)


# Fully equivalent model

nsw <- nsw %>%
  mutate(old = ifelse(nsw$age > 24, 1, 0))

summ(lm(re78 ~ treated + treated_old + old, data = nsw), digits = 3)

1250+343


# compare with
summ(rct_old, digits = 3)
summ(rct_young, digits = 3)


