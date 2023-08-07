## Familiarise self with dataset and derive naive estimator 

library(tidyverse)
library(MatchIt)
library(cobalt)

#load in dataset

nsw_psid <- read_csv("nsw_psid.csv")

dplyr::glimpse(nsw_psid)

summary(nsw_psid)

corrplot::corrplot.mixed(corr = cor(nsw_psid))

#1 Compare characteristics of NSW Treated and Non-Treated

treated_diffs <- MatchIt::matchit(treated ~ age + age2 + educ + black + 
                                    hispanic + married + nodegree + re75 + re78,
                                  data = nsw_psid)

summary(treated_diffs)
plot(summary(treated_diffs))

love.plot(treated_diffs, binary = "std")
bal.plot(treated_diffs, var.name = "age")
bal.plot(treated_diffs, var.name = "married")
plot(treated_diffs, type = "density", which.xs = c("age", "educ", "black"))

pstest0(varlist = names(nsw_psid)[2:8], treated = "treated", data = nsw_psid)

pstest0(names(nsw_psid)[2:9], treated = "treated", data = nsw_psid) |> 
  pstest_scatter(xmax = 200)


#naive estimator

nsw_psid |> 
  group_by(treated) |> 
  summarise(mean(re78))

#naive estimated is difference is therefore 5976 - 21554 = 

5976 - 21554

#Or via lm

lm(re78 ~ treated, data = nsw_psid)
