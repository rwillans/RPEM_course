## 	Barbara Sianesi
##  (barbarasianesi@gmail.com)
##	POLICY EVALUATION METHODS Course
##	
##	PRACTICAL SESSION 4a 
##	REGRESSION DISCONTINUITY DESIGN: SHARP  
##  ************************************************************************

##	Some initial R code kindly contributed by Anas Abu (formerly at DCMS)
## 	Any mistakes remain mine (Barbara)
##	In the spirit of R, if you can improve this script, please let me know :)


#install.packages("rdrobust")
#install.packages("estimatr")
#install.packages("rdd")
library(rdrobust)
library(tidyverse)
library(dplyr)
library(MatchIt)
library(estimatr)
library(jtools)
source("pstest0.R")


#setwd("your working directory")

nsw_psid_rdd <- read.csv("nsw_psid_rdd.csv")

str(nsw_psid_rdd)

summary(nsw_psid_rdd)


#  a) Eligibles, characteristics and outcomes within discontinuity samples
# ***************************************************************************

# Eligibility indicator

nsw_psid_rdd <- mutate(nsw_psid_rdd, eligible = ifelse(re75<=1000, 1, 0))
	

# Discontinuity samples indicators

ds5_subset <- nsw_psid_rdd %>% 
  filter(re75>=(1000-500) & re75<=(1000+500))

ds2_subset <- nsw_psid_rdd %>% 
  filter(re75>=(1000-200) & re75<=(1000+200))

ds1_subset <- nsw_psid_rdd %>% 
  filter(re75>=(1000-100) & re75<=(1000+100))

treated <- "eligible"
covariates <- c("age", "black")


summary(nsw_psid_rdd$eligible)
addmargins(table(nsw_psid_rdd$eligible))
pstest0(varlist = covariates, treated = treated, data = nsw_psid_rdd)
summ(lm(re78 ~ eligible, data = nsw_psid_rdd), digits = 3)

summary(ds5_subset$eligible)
addmargins(table(ds5_subset$eligible))
pstest0(varlist = covariates, treated = treated, data = ds5_subset)
summ(lm(re78 ~ eligible, data = ds5_subset), digits = 3)

summary(ds2_subset$eligible)
addmargins(table(ds2_subset$eligible))
pstest0(varlist = covariates, treated = treated, data = ds2_subset)
summ(lm(re78 ~ eligible, data = ds2_subset), digits = 3)

summary(ds1_subset$eligible)
addmargins(table(ds1_subset$eligible))
pstest0(varlist = covariates, treated = treated, data = ds1_subset)
summ(lm(re78 ~ eligible, data = ds1_subset), digits = 3)


# b) RDD plots
# ************************************************************************

# P(D=1 | Z)

rdplot(nsw_psid_rdd$eligible, nsw_psid_rdd$re75, c = 1000, subset = nsw_psid_rdd$re75 < 2000 )


# E(Y | Z)

rdplot(nsw_psid_rdd$re78, nsw_psid_rdd$re75, c = 1000)
rdplot(nsw_psid_rdd$re78, nsw_psid_rdd$re75, c = 1000, subset = nsw_psid_rdd$re75 < 2000 )


# Trying out different bin choices...

rdplot(nsw_psid_rdd$re78, nsw_psid_rdd$re75, c = 1000, nbins = c(20,20),   subset = nsw_psid_rdd$re75 < 2000) 
rdplot(nsw_psid_rdd$re78, nsw_psid_rdd$re75, c = 1000, nbins = c(150,150), subset = nsw_psid_rdd$re75 < 2000)
rdplot(nsw_psid_rdd$re78, nsw_psid_rdd$re75, c = 1000, binselect = "es",   subset = nsw_psid_rdd$re75 < 2000) 
rdplot(nsw_psid_rdd$re78, nsw_psid_rdd$re75, c = 1000, binselect = "qsmv", subset = nsw_psid_rdd$re75 < 2000)


# Playing around with the order of polynomial used to approximate the population conditional mean functions 
# for treated and non-treated

rdplot(nsw_psid_rdd$re78, nsw_psid_rdd$re75, c = 1000, p=1, subset = nsw_psid_rdd$re75 < 2000) 
rdplot(nsw_psid_rdd$re78, nsw_psid_rdd$re75, c = 1000, p=2, subset = nsw_psid_rdd$re75 < 2000) 
rdplot(nsw_psid_rdd$re78, nsw_psid_rdd$re75, c = 1000, p=3, subset = nsw_psid_rdd$re75 < 2000) 


# c) SRD estimates - parametric, modelling E(Y0|Z)
# ************************************************************************

# Get higher order terms 

nsw_psid_rdd <- nsw_psid_rdd %>%
  mutate(re75_2 = re75^2, 
  		   re75_3 = re75^3, 
  		   re75_4 = re75^4,
         re75_5 = re75^5)

# Linear model
summ(lm(re78 ~ eligible + re75, data = nsw_psid_rdd), digits = 3)

# Quadratic model
summ(lm(re78 ~ eligible + re75 + re75_2, data = nsw_psid_rdd), digits = 3)

# Cubic model
summ(lm(re78 ~ eligible + re75 + re75_2 + re75_3, data = nsw_psid_rdd), digits = 3)

# 4th-order polynomial model
summ(lm(re78 ~ eligible + re75 + re75_2 + re75_3 + re75_4, data = nsw_psid_rdd), digits = 3)

# 5th-order polynomial model
summ(lm(re78 ~ eligible + re75 + re75_2 + re75_3 + re75_4 + re75_5, data = nsw_psid_rdd), digits = 3)

# Controlling for X in 4th-order polynomial model
summ(lm(re78 ~ eligible + re75 + re75_2 + re75_3 + re75_4 + age + black, 
        data = nsw_psid_rdd), digits = 3)


# In discontinuity samples
# Note: we drop higher order terms

ds2_subset <- nsw_psid_rdd %>% 
  filter(re75>=(1000-200) & re75<=(1000+200))

ds1_subset <- nsw_psid_rdd %>% 
  filter(re75>=(1000-100) & re75<=(1000+100))


summ(lm(re78 ~ eligible + re75 + re75_2, 
        data = ds2_subset), digits = 3)
summ(lm(re78 ~ eligible + re75, 
        data = ds2_subset), digits = 3)

summ(lm(re78 ~ eligible + re75,                       
        data = ds1_subset), digits = 3)
summ(lm(re78 ~ eligible,                                           
        data = ds1_subset), digits = 3)

    

# d) SRD estimates - parametric, modelling E(Y0|Z) & E(Y1|Z)
# ************************************************************************
# 2nd-order polynomial

nsw_psid_rdd <- nsw_psid_rdd %>%
  mutate(Zc = re75 - 1000,  
		 Zc_2   = Zc^2,	
	 	 DZc    = eligible*Zc,
		 DZc_2  = eligible*Zc_2)

summ(lm(re78 ~ eligible + Zc + Zc_2 + DZc + DZc_2, data = nsw_psid_rdd), digits = 3)



# e) SRD estimates - non-parametric LLR
# ************************************************************************

# Fully data-driven RD treatment-effects estimation and inference

summary(rdrobust(nsw_psid_rdd$re78, nsw_psid_rdd$re75, c = 1000))

# Take MINUS the effect as discontinuity the other way round
# ie treatment is assumed to jump from 0 to 1 at z0=1000


# Experimenting with different bandwidths 
summary(rdrobust(nsw_psid_rdd$re78, nsw_psid_rdd$re75, c = 1000, bwselect = "msetwo"))
summary(rdrobust(nsw_psid_rdd$re78, nsw_psid_rdd$re75, c = 1000, bwselect = "certwo"))



#  d) Specification checks
# ************************************************************************

# Balancing of covariates **************************************
summary(rdrobust(nsw_psid_rdd$age,   nsw_psid_rdd$re75, c = 1000, bwselect = "certwo"))
summary(rdrobust(nsw_psid_rdd$black, nsw_psid_rdd$re75, c = 1000, bwselect = "certwo"))



# Placebo discontinuities *************************************** 
nsw_psid_rdd %>% 
  filter(re75<=1000) %>% 
  summary(re75)

rdplot(nsw_psid_rdd$re78, nsw_psid_rdd$re75, c = 710, subset = nsw_psid_rdd$re75 < 2000) 
summary(rdrobust(nsw_psid_rdd$re78, nsw_psid_rdd$re75, c = 710))
  
nsw_psid_rdd %>% 
  filter(re75>1000 & re75<=2000) %>% 
  summary(re75)

rdplot(nsw_psid_rdd$re78, nsw_psid_rdd$re75, c = 1600, subset = nsw_psid_rdd$re75 < 2000) 
summary(rdrobust(nsw_psid_rdd$re78, nsw_psid_rdd$re75, c = 1600))



# Testing for bunching of Z at z0 ************************************* 

#install.packages("rdd")
library(rdd)

DCdensity(nsw_psid_rdd$re75, cutpoint = 1000)


    


