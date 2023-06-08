## 	Barbara Sianesi
##  (barbarasianesi@gmail.com)
##	POLICY EVALUATION METHODS Course
##	
##	PRACTICAL SESSION 6
##  MATCHING METHODS  
##  ************************************************************************

##	Some initial R code kindly contributed by Anas Abu (formerly at DCMS)
## 	Any mistakes remain mine (Barbara)
##	In the spirit of R, if you can improve this script, please let me know :)



#install.packages("MatchIt")
library(MatchIt)
library(cobalt)
library(tidyverse)
source("pstestM.R")
source("pstest_scatter.R")

#setwd("your working directory")

nsw_psid <- read.csv("nsw_psid.csv")


# ATNT      					      
# *******


nsw_psid <- nsw_psid %>%
  mutate(treat0 =  1 - treated) 

covariates <- c("age", "educ", "black", "hispanic", "married", "nodegree", "re75")


score_mod <- glm(treat0 ~ age + educ + black + hispanic + married + nodegree + re75, 
                  family = binomial(link = "probit"), 
                  data = nsw_psid)

summary(score_mod)

nsw_psid <- nsw_psid %>% 
  mutate(pscore = score_mod$fitted.values)

# histogram & boxplot
bal.plot(nsw_psid, data = nsw_psid, treat = "treat0", var.name = "pscore", type = "histogram", mirror = TRUE, bins = 50)

library(ggplot2)
ggplot(nsw_psid, aes(x = as.factor(treat0), y = pscore)) + geom_boxplot()


# Nearest neighbour with replacement without caliper         
# *****************************************************

m.out <- matchit(treat0 ~ age + educ + black + hispanic + married + nodegree + re75,
					         data = nsw_psid, 
					         method = "nearest", distance = "glm", link = "probit", replace = TRUE)
m.out
summary(m.out, improvement = TRUE)
plot(summary(m.out))
plot(m.out, type = "density", which.xs = c("age", "educ", "re75"))

m_data <- match.data(m.out, drop.unmatched = FALSE)
bias_m <- pstestM(varlist = covariates, treated = "treat0", data = m_data)
bias_m
# ck out max bias above to decide on xmax:
pstest_scatter(data=bias_m, xmax=120)


# Get effect:
summary(lm(re78 ~ treat0, data = m_data, weights = weights))
# Remember to take -1*(effect) for ATNT



# Nearest neighbour with replacement with common support at the boundaries         
# **************************************************************************
m.out <- matchit(treat0 ~ age + educ + black + hispanic + married + nodegree + re75,
                 data = nsw_psid, 
                 method = "nearest", distance = "glm", link = "probit", replace = TRUE,
                 discard = "treated")

summary(m.out)
m_data <- match.data(m.out, drop.unmatched = FALSE)
bias_m <- pstestM(varlist = covariates, treated = "treat0", data = m_data)
bias_m
pstest_scatter(data=bias_m, xmax=80)
summary(lm(re78 ~ treat0, data = m_data, weights = weights))



# Mahalanobis-metric matching 			
# *****************************************************
m.out <- matchit(treat0 ~ age + educ + black + hispanic + married + nodegree + re75,
					         data = nsw_psid, 
					         distance = "mahalanobis", replace = TRUE)

m_data <- match.data(m.out, drop.unmatched = FALSE)
bias_m <- pstestM(varlist = covariates, treated = "treat0", data = m_data)
bias_m
pstest_scatter(data=bias_m, xmax=120)
summary(lm(re78 ~ treat0, data = m_data, weights = weights))


# Augmented Mahalanobis-metric matching 			
# *****************************************************
m.out <- matchit(treat0 ~ pscore + age + educ + black + hispanic + married + nodegree + re75,
					          data = nsw_psid, 
					          distance = "mahalanobis", replace = TRUE)

m_data <- match.data(m.out, drop.unmatched = FALSE)
bias_m <- pstestM(varlist = covariates, treated = "treat0", data = m_data)
bias_m
pstest_scatter(data=bias_m, xmax=120)
summary(lm(re78 ~ treat0, data = m_data, weights = weights))



# ATNT directly	(called "ATC")
# *****************************
# Nearest neighbour with replacement without caliper         

m.out_atnt <- matchit(treated ~ age + educ + black + hispanic + married + nodegree + re75,
						            data = nsw_psid, 
						            method = "nearest", distance = "glm", link = "probit", replace = TRUE, 
						            estimand = "ATC")
m.out_atnt
summary(m.out_atnt)
m_data <- match.data(m.out_atnt, drop.unmatched = FALSE)
bias_m <- pstestM(varlist = covariates, treated = "treat0", data = m_data)
bias_m
summary(lm(re78 ~ treated, data = m_data, weights = weights))
# Note sign is correct 


# Compare with film
source("film.R")
film(data = nsw_psid, outcome = "re78", treated = "treated", covariates = covariates, 
      robust = TRUE, effect = "ATNT")

