

setwd("D:/Documents/R/My_R_Practicals")
source("pstest0.R")
source("pstestM.R")
source("pstest_scatter.R")


# Set the input arguments
nsw_psid <- read.csv("nsw_psid.csv")
data <- nsw_psid
treated <- "treated"
covariates <- c("age", "educ", "black", "hispanic", "married", "nodegree", "re75")

# Check balancing in the raw samples
pstest0(varlist = covariates, treated = treated, data = data)
#pstest0(varlist = c("age", "educ", "black", "hispanic", "married", "nodegree", "re75"), treated = "treated", data = nsw_psid)


# Matching & checking balancing on matched samples
library(MatchIt)
m.out <- matchit(treated ~ age + educ + black + hispanic + married + nodegree + re75,
                 data = nsw_psid, method = "nearest", distance = "glm", link = "probit", replace = TRUE)

m_data <- match.data(m.out, drop.unmatched = FALSE)

pstestM(varlist = covariates, treated = treated, data = m_data)



# Scatter plot
bias <- pstest0(varlist = covariates, treated = treated, data = data)
# ck out max bias to decide on xmax
bias
pstest_scatter(data = bias, xmax=200)

bias_m <- pstestM(varlist = covariates, treated = treated, data = m_data)
# ck out max bias to decide on xmax
bias_m
# or for comparison might want to stick to xmax on raw data
pstest_scatter(data = bias_m, xmax=20)


