## 	Barbara Sianesi
##  (barbarasianesi@gmail.com)
##	POLICY EVALUATION METHODS Course
##	
##	PRACTICAL SESSION 8 
##  SYNTHETIC CONTROL METHOD 
##  ************************************************************************

##	In the spirit of R, if you can improve this script, please let me know :)



#install.packages("Synth")
library(Synth)
library(tidyverse)

#setwd("your working directory")

smoking <- read.csv("smoking.csv")

# need to assign numbers to each state 

smoking <- smoking %>%
  group_by(state) %>%
  mutate(state.num = cur_group_id()) 

smoking <- as.data.frame(smoking)


# 1. Prep data for synth
dataprep.out <- 
  dataprep(foo = smoking, 
           unit.variable = "state.num",
           unit.names.variable = "state",
           time.variable = "year",
           dependent = "cigsale",
           predictors = c("lnincome", "retprice", "age15to24"), 
           special.predictors = list(
             list("cigsale", 1988, "mean"),
             list("cigsale", 1980, "mean"),
             list("cigsale", 1975, "mean"),
             list("beer", 1984:1988, "mean")
           ),
           treatment.identifier = 3, # California
           controls.identifier = c(1,2, 4:39),
           time.predictors.prior = c(1980:1988), 
           time.optimize.ssr = c(1970:1988), # Pre-treatment periods 
           time.plot = c(1970:2000)
		   ) 


# 2. Construct synthetic control group
synth.out <- synth(dataprep.out)


# 3. Model diagnostics & results
synth.tables <- synth.tab(dataprep.res = dataprep.out, synth.res = synth.out)
print(synth.tables)


# Graph the outcome paths of treated vs synthetic control
path.plot(dataprep.res = dataprep.out,synth.res = synth.out, tr.intake = 1988, Ylab = c(""))


# Graph difference between the two (gaps)
gaps.plot(dataprep.res = dataprep.out,synth.res = synth.out, tr.intake = 1988, 
          Main = c("Gaps: Treated - Synthetic"),
          Ylab = c("")
          )

		  
# List California's outcome
dataprep.out$Y1plot

# List Synthetic California's outcome
dataprep.out$Y0plot %*% synth.out$solution.w

# List difference between the two (gaps)
gaps <- dataprep.out$Y1plot-(dataprep.out$Y0plot %*% synth.out$solution.w) 
gaps



## Using -tidysynth- (especially useful for inference)
## ************************************************************************

#install.packages('tidysynth')
library(tidysynth)
library(tidyverse)

smoking <- read.csv("smoking.csv")


# Set up everything & construct the synthetic control

smoking_out <-
  
  smoking %>%
  
  # set up the synthetic control object
  synthetic_control(outcome = cigsale, 
                    unit = state, 
                    time = year, 
                    i_unit = "California",
                    i_time = 1988, 
                    generate_placebos = TRUE # for inference later
  ) %>%
  
  # Generate the aggregate predictors, averaged over appropriate time frames 
  generate_predictor(time_window = 1980:1988,
                     ln_income   = mean(lnincome,  na.rm = TRUE),
                     ret_price   = mean(retprice,  na.rm = TRUE),
                     youth       = mean(age15to24, na.rm = TRUE)) %>%
  
  generate_predictor(time_window = 1984:1988, beer_sales  = mean(beer, na.rm = TRUE)) %>%
  generate_predictor(time_window = 1975, cigsale_1975 = cigsale) %>%
  generate_predictor(time_window = 1980, cigsale_1980 = cigsale) %>%
  generate_predictor(time_window = 1988, cigsale_1988 = cigsale) %>%
  
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = 1970:1988,   
                   margin_ipop = .02, sigf_ipop = 7, bound_ipop = 6   # optimizer options
  ) %>%
  
  # Generate the synthetic control
  generate_control()


# Weighting of units and variables
smoking_out %>% plot_weights()


# Comparability of synthetic control to treated unit
smoking_out %>% grab_balance_table()


# Graph comparing paths
smoking_out %>% plot_trends()


# Graph of difference between the two
smoking_out %>% plot_differences()


# INFERENCE

# Plot placebo runs
smoking_out %>% plot_placebos()
smoking_out %>% plot_placebos(prune = FALSE)

# Plot (post-MSPE / pre-MSP)
smoking_out %>% plot_mspe_ratio()

