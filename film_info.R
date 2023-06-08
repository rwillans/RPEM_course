
setwd("D:/Documents/MyPracticals_R_WorkOn/My_R_Practicals")
source("film.R")


nsw_psid <- read.csv("nsw_psid.csv")
data <- nsw_psid
outcome <- "re78"
treated <- "treated"
covariates <- c("age", "educ", "black", "hispanic", "married", "nodegree", "re75")

film(data = data, outcome = outcome, treated = treated, covariates = covariates, robust = FALSE)
film(data = data, outcome = outcome, treated = treated, covariates = covariates, robust = TRUE)

# If happy to commit on the order of the args:
film(data, outcome, treated, covariates, robust = FALSE)
film(data, outcome, treated, covariates, robust = TRUE)

cov <- c("age", "educ")
lcov <- c("black", "hispanic", "married", "nodegree", "re75")

film(data = data, outcome = outcome, treated = treated, covariates = cov, lin_cov = lcov, robust = FALSE)
film(data = data, outcome = outcome, treated = treated, covariates = cov, lin_cov = lcov, robust = TRUE)

# ATNT
film(data = data, outcome = outcome, treated = treated, covariates = covariates, robust = FALSE, effect = "ATNT")
film(data = data, outcome = outcome, treated = treated, covariates = covariates, robust = TRUE, effect = "ATNT")

film(data = data, outcome = outcome, treated = treated, covariates = cov, lin_cov = lcov, robust = FALSE, effect = "ATNT")
film(data = data, outcome = outcome, treated = treated, covariates = cov, lin_cov = lcov, robust = TRUE, effect = "ATNT")

# ATE
film(data = data, outcome = outcome, treated = treated, covariates = covariates, robust = FALSE, effect = "ATE")
film(data = data, outcome = outcome, treated = treated, covariates = covariates, robust = TRUE, effect = "ATE")

film(data = data, outcome = outcome, treated = treated, covariates = cov, lin_cov = lcov, robust = FALSE, effect = "ATE")
film(data = data, outcome = outcome, treated = treated, covariates = cov, lin_cov = lcov, robust = TRUE, effect = "ATE")


