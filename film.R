## 	Barbara Sianesi
##
##	This is my translation into R of part of Edwin's and mine Stata film ado
##
##	film runs a fully interacted linear model of outcome on treated, covariates X and 
##  all X*treated interactions
##
##  Calculates the ATT by default, unless
##  effect = "ATNT" or effect = "ATE" is specified
##
##  Syntax:
##  film(data, outcome, treated, covariates, lin_cov = NULL, robust = FALSE, effect = "ATT")
##  where lin_cov is a set of covariates the users does not wish to interact with treated
##
##	Needs 
##  library(estimatr)
##  library(AER)
## 
##	This version: 11 March 2023
##  ************************************************************************************************************************************

library(estimatr)
library(AER)

film <- function(data, outcome, treated, covariates, lin_cov = NULL, robust = FALSE, effect = "ATT") {
  
  # Compute interaction terms between treatment and covariates' means within treated group
  inter_terms <- c()
  for (i in covariates) {
    if (effect == "ATT") {
      inter_mean <- mean(data[data[[treated]] == 1, i])
    } else if (effect == "ATNT") {
      inter_mean <- mean(data[data[[treated]] == 0, i])
    } else if (effect == "ATE") {
      inter_mean <- mean(data[[i]])
    }
    inter_term <- data[[treated]] * (data[[i]] - inter_mean)
    inter_terms <- c(inter_terms, inter_term)
  }
  
  inter_terms_names <- paste0(treated, "X", covariates)
  inter_terms <- setNames(as.data.frame(matrix(inter_terms, ncol = length(covariates))), inter_terms_names)
  
  # Combine data and interaction terms
  data <- cbind(data, inter_terms)
  
  # Construct formula and run regression
  formula <- as.formula(paste(outcome, "~", treated, "+", paste(covariates, collapse = "+"), 
                              if (!is.null(lin_cov)) paste("+", paste(lin_cov, collapse = "+")) else "", 
                              "+", paste0(treated, "X", covariates, collapse = "+")))
  
  if (robust) {
    model <- lm_robust(formula = formula, data = data)
    
  } else {
    model <- lm(formula = formula, data = data)
  }
  
  # Extract coefficients, standard errors, t-statistics, and p-values
  est <- coef(model)[treated]
  se <- summary(model)$coef[treated, 2]
  tstat <- est / se
  p_value <- round(summary(model)$coef[treated, 4], 3)
  
  # Extract F-statistic and p-value for joint significance of interaction terms
  hypo <- linearHypothesis(model, c(paste0(treated, "X", covariates)), test = "F")
  F_statistic <- hypo$F[2]
  F_p_value   <- hypo$"Pr(>F)"[2]
  
  # Extract interaction coefficients and p-values
  inter_est <- c()
  inter_p <- c()
  for (inter in inter_terms_names) {
    inter_est <- c(inter_est, round(coef(model)[inter], 2))
    inter_p <- c(inter_p, round(summary(model)$coef[inter, 4], 3))
  }
  
  # Add interaction coefficients and p-values to data frame
  df_results <- data.frame(estimate = format(round(est, 2), nsmall = 2),
                           std_error = format(round(se, 2), nsmall = 2),
                           t_value = format(round(tstat, 2), nsmall = 2),
                           p_value = p_value,
                           F_statistic = format(round(F_statistic, 2), nsmall = 2),
                           F_p_value = round(F_p_value, 3))
  
  interactions_df <- data.frame(coefficient = inter_est,
                                p_value = inter_p)
  
  # Print results
  if (effect == "ATT") {
    cat("ATT")
  } else if (effect == "ATNT") {
    cat("ATNT")
  } else if (effect == "ATE") {
    cat("ATE")
  }

  print(df_results)
  print(interactions_df)
  
}

