## 	Barbara Sianesi
##  (barbarasianesi@gmail.com)
##	POLICY EVALUATION METHODS Course
##	
##	PRACTICAL SESSION 7
##  LONGITUDINAL METHODS: BA & DiD    
##  ************************************************************************

##	Some initial R code kindly contributed by Anas Abu (formerly at DCMS)
## 	Any mistakes remain mine (Barbara)
##	In the spirit of R, if you can improve this script, please let me know :)

library(estimatr)
library(tidyverse)
library(AER)
library(MatchIt)
library(jtools)

# *****************************
# Before-After   					
# *****************************

#setwd("your working directory")

nsw_psid <- read.csv("nsw_psid.csv")

nsw_psid <- nsw_psid %>%
  mutate(dre = re78 - re75)  

summ(lm(dre ~ 1, data = nsw_psid, treated == 1), digits = 3)


# ******************************
# Difference-in-Differences  				
# ******************************


nsw_psid <- read.csv("nsw_psid.csv")

nsw_psid <- nsw_psid %>%
  mutate(dre = re78 - re75)  

summ(lm(dre ~ treated, data = nsw_psid), digits = 3)


# Allowing trend to depend on age	

summ(lm(dre ~ treated + age + age2, data = nsw_psid), digits = 3)


# Allowing trend to depend on pre-program earnings:

summ(lm(dre ~ treated + re75, data = nsw_psid), digits = 3)

# Be aware it's the same as running a regression on the level!

summ(lm(re78 ~ treated + re75, data = nsw_psid), digits = 3)


# Now replicating DiD estimate controlling (linearly) for age by stacking the data

did_panel <- lm_robust(dre ~ treated + age, data = nsw_psid)

summary(did_panel)


# Stacking the data

nsw_psid <- nsw_psid %>%
  mutate(nsw_psid, ID = row_number()) %>% # generating a new variable which is just the number of observations
  slice(rep(1:n(), each = 2))
  
nsw_psid$time <- rep(c(1, 0), length.out=nrow(nsw_psid)) # this makes the first observation of each person time=1, and the second person time=0. Done by making odd rows time=1 and even rows time=0

nsw_psid <- nsw_psid %>%
  mutate(re   = ifelse(time == 1, re78, re75),
  		   DT   = treated*time, 
  		   ageT = age*time)

nsw_psid[1:10, c(12, 1, 2, 9, 10, 13, 14, 15, 16)]

did_xs <- lm_robust(re ~ DT + treated + time + age + ageT, data = nsw_psid, clusters = ID) 

summary(did_xs)

summary(did_panel)


# Compare also:
did_xs1 <- lm_robust(re ~ DT + treated + time + age, data = nsw_psid, clusters = ID) 
did_xs2 <- lm_robust(re ~ DT + treated + time,       data = nsw_psid, clusters = ID) 

summary(did_xs1)
summary(did_xs2)



# Back to the original nsw_psid data 

nsw_psid <- read.csv("nsw_psid.csv")

nsw_psid <- nsw_psid %>%
  mutate(dre = re78 - re75)  

  
# Full model - parametric
# ***********************************

did_full <- lm_robust(dre ~ treated + age + educ + black + hispanic + married + nodegree, data = nsw_psid)

summary(did_full)

# Full model - parametric with heterogeneous impacts: 
# we can directly use -film- on dre
# *******************************************************
source("film.R")

covariates <- c("age", "black", "hispanic", "married", "educ", "nodegree")

film(data = nsw_psid, outcome = "dre", treated = "treated", 
     covariates = covariates, robust = TRUE)

cov  <- c("age")
lcov <- c("black", "hispanic", "married", "nodegree", "educ")

film(data = nsw_psid, outcome = "dre", treated = "treated", 
     covariates = cov, lin_cov = lcov, robust = TRUE)


# Full model - non-parametric	
# ***********************************

# Note: we already know we can balance the samples (for the ATT!)


# Nearest neighbour

m.out1 <- matchit(treated ~ age + educ + black + hispanic + married + nodegree,
				          data = nsw_psid, 
				          method = "nearest", distance = "glm", link = "probit", replace = TRUE)
m.out1

m_data1 <- match.data(m.out1)

summary(lm(dre ~ treated, data = m_data1, weights = weights))


# Mahal matching

m.out2 <- matchit(treated ~ age + educ + black + hispanic + married + nodegree,
				            data = nsw_psid, 
				            distance = "mahalanobis", replace = TRUE)
m.out2


m_data2 <- match.data(m.out2, drop.unmatched = FALSE)
covariates <- c("age", "educ", "black", "hispanic", "married", "nodegree")
source("pstestM.R")
source("pstest_scatter.R")
bias_m <- pstestM(varlist = covariates, treated = "treated", data = m_data2)
bias_m
pstest_scatter(data=bias_m, xmax=10)

summary(lm(dre ~ treated, data = m_data2, weights = weights))



