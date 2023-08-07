## 	Barbara Sianesi
##
##	Example call:
##	result <- pstest0(varlist = covariates, treated = treated, data = data)
##	pstest_scatter(data=result, xmax=200)
##
##	result_m <- pstestM(varlist = covariates, treated = treated, data = m_data)
##	pstest_scatter(data=result_m, xmax=20)
##
##	This version: 10 March 2023
##  *************************************************************************************************************************************

## RJW added "var_labels" text to add labels to graph

pstest_scatter <- function(data, xmax) {

  library(ggplot2)

  bias_num 	  <- as.numeric(data$bias)
  V_ratio_num <- as.numeric(data$V_e.ratio)
  by_num      <- abs(xmax)/4
  var_labels <- data$variable
  
  
  p <- ggplot(data=data, aes(x=bias_num, y=V_ratio_num, label=var_labels)) +
    geom_point(color="#0072B2") +
    theme_bw() +
    scale_x_continuous(limits=c(-xmax, xmax), breaks=seq(-xmax, xmax, by=by_num), expand=c(0, 0)) +
    scale_y_continuous(limits=c(0, NA), breaks=seq(0, 2, by=0.5), expand=c(0, 0)) +
    labs(x="Standardized % bias", y="Variance ratio of residuals") +
    geom_hline(yintercept=1, color="#555555", linewidth=1.2, linetype="solid") +
    geom_hline(yintercept=c(0.8, 1.25), color="#555555", linewidth=0.8, linetype="dashed") +
    geom_hline(yintercept=c(0.5, 2), color="#555555", linewidth=0.8, linetype="dotted") +
    geom_vline(xintercept=0, color="#555555", linewidth=1.2, linetype="solid") + 
    geom_text(check_overlap = TRUE, vjust = 1, nudge_y = -0.01)

  print(p)

}






