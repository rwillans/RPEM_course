## 	Barbara Sianesi
##
##	This is my translation into R of part of Edwin's and mine Stata pstest ado
##	pstestM provides balancing indicators between two matched groups
##
##	Note: Assumes matching has been performed via MatchIt or other function
##	that leaves behind a weights variable
##
##	Consider also the companion pstest_scatter.R 
##
##  Example call:
##	m.out  <- matchit(....)
##	m_data <- match.data(m.out, drop.unmatched = FALSE)
##	pstestM(varlist = c("age", "educ", "black", "hispanic", "married", "nodegree", "re75"), treated = "treated", data = m_data)
##
##	Needs package matrixStats
##
##	This version: 10 March 2023
##  ************************************************************************************************************************************

library(matrixStats)

pstestM <- function(varlist, treated, data) {

  results <- data.frame(variable=character(), treated=double(), control=double(), bias=character(), b_reduc=character(), `V_e ratio`=double(), t_stat=double(), p_value=double(), stringsAsFactors=FALSE)
  
  full_mod <- glm(paste(treated, paste(varlist, collapse="+"), sep="~"), 
                 data = data, weights = data$weights, 
                 family = binomial(link = "probit"))
  chi_stat <- summary(full_mod)$null.deviance - summary(full_mod)$deviance
  p_value  <- pchisq(chi_stat, df = summary(full_mod)$df.null - summary(full_mod)$df.residual, lower.tail = FALSE)
  # Note: no R2 available from glm: summary(full_mod)$r.squared

  # Compute propensity score index
  ps_index <- predict(full_mod, type = "link")
  
  # Compute Rubin's B & R
  m1 <- weighted.mean(ps_index[data[[treated]] == 1], w=data$weights[data[[treated]] == 1], na.rm = TRUE)
  m0 <- weighted.mean(ps_index[data[[treated]] == 0], w=data$weights[data[[treated]] == 0], na.rm = TRUE)
  v1 <- weightedVar(ps_index[data[[treated]] == 1], w=data$weights[data[[treated]] == 1], na.rm = TRUE)
  v0 <- weightedVar(ps_index[data[[treated]] == 0], w=data$weights[data[[treated]] == 0], na.rm = TRUE)
  rubinB <- 100 * abs(m1 - m0) / sqrt((v1 + v0) / 2)
  starB <- ifelse(rubinB >= 25, "*", "")
  rubinR <- v1 / v0
  starR <- ifelse(between(rubinR, 0.5, 2), "", "*")
  
  for(v in varlist) {
    v_numeric <- as.numeric(data[[v]])
	
    # Compute t-test
    model <- lm(v_numeric ~ data$treated, data = data, weights = data$weights)
    t_stat <- summary(model)$coefficients[2, 3]
    p_value <- summary(model)$coefficients[2, 4]

	# Compute standardised % bias - before
	m1u <- mean(v_numeric[data[[treated]] == 1], na.rm = TRUE)
    v1u <- var(v_numeric[data[[treated]] == 1], na.rm = TRUE)
    m0u <- mean(v_numeric[data[[treated]] == 0], na.rm = TRUE)
    v0u <- var(v_numeric[data[[treated]] == 0], na.rm = TRUE)
    biasu <- 100 * (m1u - m0u) / sqrt((v1u + v0u) / 2)

	# Compute standardised % bias - after
	m1m <- weighted.mean(v_numeric[data[[treated]] == 1], w=data$weights[data[[treated]] == 1], na.rm = TRUE)
	m0m <- weighted.mean(v_numeric[data[[treated]] == 0], w=data$weights[data[[treated]] == 0], na.rm = TRUE)
    biasm <- 100 * (m1m - m0m) / sqrt((v1u + v0u) / 2)

	# Compute % reduction in absolute bias
	b_reduc = -100*(abs(biasm) - abs(biasu))/abs(biasu)
	
	# Compute Rubin's variance ratio of the residuals
    lm1 <- lm(v_numeric[data[[treated]] == 1] ~ ps_index[data[[treated]] == 1], weights = data$weights[data[[treated]] == 1])
    lm0 <- lm(v_numeric[data[[treated]] == 0] ~ ps_index[data[[treated]] == 0], weights = data$weights[data[[treated]] == 0])
    ve_ratio <- summary(lm1)$sigma^2 / summary(lm0)$sigma^2
    
    if ((ve_ratio > 1.25 & ve_ratio <= 2) | (ve_ratio < 0.8 & ve_ratio >= 0.5)) {
      star_ratio <- "*"
    } else if (ve_ratio > 2 | ve_ratio < 0.5) {
      star_ratio <- "**"
    } else {
      star_ratio <- ""
    }
    
    result_row <- data.frame(variable=v, treated=sprintf("%.2f", m1m), control=sprintf("%.2f", m0m), bias=sprintf("%.1f", biasm), b_reduc=sprintf("%.1f", b_reduc), t_stat=sprintf("%.2f", t_stat), p_value=sprintf("%.4f", p_value), `V_e ratio`=sprintf("%.2f", ve_ratio), star_ratio = ifelse(is.na(ve_ratio) | between(ve_ratio, 0.8, 1.25), "", ifelse(between(ve_ratio, 0.5, 0.8) | between(ve_ratio, 1.25, 2), "*", "**")), stringsAsFactors=FALSE)
	results <- rbind(results, result_row)
  }
  
  mean_abs_bias <- mean(abs(as.numeric(results$bias)), na.rm = TRUE)
  median_abs_bias <- median(abs(as.numeric(results$bias)), na.rm = TRUE)
  cat("Chi-statistic: ", sprintf("%.3f", chi_stat), "\n")
  cat("P-value: ", sprintf("%.3f", p_value), "\n")
  cat("Rubin's B: ", sprintf("%.1f", rubinB), starB, "\n")
  cat("Rubin's R: ", sprintf("%.2f", rubinR), starR, "\n")
  cat("Mean absolute bias: ", sprintf("%.1f", mean_abs_bias), "\n")
  cat("Median absolute bias: ", sprintf("%.1f", median_abs_bias), "\n")
  
  return(results)
}



