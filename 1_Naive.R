## 	Barbara Sianesi
##  (barbarasianesi@gmail.com)
##	POLICY EVALUATION METHODS Course
##	
##	PRACTICAL SESSION 1 
##  NAIVE NON-EXPERIMENTAL ESTIMATOR  
##  ************************************************************************

##	Some initial R code kindly contributed by Anas Abu (formerly at DCMS)
##  Thank you also to Alex Stroud (DCMS)
## 	Any mistakes remain mine (Barbara)
##	In the spirit of R, if you can improve this script, please let me know :)


## 	Get acquainted with the data
##  ************************************************************************

#setwd("your working directory")

nsw_psid <- read.csv("nsw_psid.csv")

str(nsw_psid)

summary(nsw_psid)

summary(nsw_psid$treated)

addmargins(table(nsw_psid$treated))


## 1. Compare the characteristics of the NSW treated group
##	  and the non-experimental PSID comparison group      			
## ************************************************************************

#install.packages("MatchIt")
#install.packages("cobalt")
library(MatchIt)
library(cobalt)


raw_bal <- matchit(treated ~ age + educ + black + hispanic + married + nodegree + re75,
					 data = nsw_psid, method = NULL)

summary(raw_bal)
plot(summary(raw_bal))
love.plot(raw_bal, binary = "std")
bal.plot(raw_bal, var.name = "re75")
plot(raw_bal, type = "density", which.xs = c("age", "educ", "black"))

# Let's try with my pstest for R
# Make sure the scripts are in your working directory
source("pstest0.R")
source("pstest_scatter.R")

treated <- "treated"
covariates <- c("age", "educ", "black", "hispanic", "married", "nodegree", "re75")

pstest0(varlist = covariates, treated = treated, data = nsw_psid)

# Scatter plot
result <- pstest0(varlist = covariates, treated = treated, data = nsw_psid)
# ck out max bias do decide on xmax
result
pstest_scatter(data=result, xmax=200)

library(ggplot2)
ggplot(nsw_psid, aes(x = as.factor(treated), y = re75)) + geom_boxplot()


## 2. Naive non-experimental estimator   				      
## ************************************************************************

naive_eY1 <- mean(nsw_psid$re78[nsw_psid$treated == "1"])
naive_eY1

naive_eY0 <- mean(nsw_psid$re78[nsw_psid$treated == "0"])
naive_eY0

naive_eY1 - naive_eY0

#install.packages("jtools")
library(jtools)
naive_mod <- lm(re78 ~ treated, data = nsw_psid)
summ(naive_mod, digits = 3)


