library(tidyverse)
library(MatchIt)
library(cobalt)
library(AER)
source('pstest0.R')
source('pstest_scatter.R')
options(scipen = 999)


# Load dataset

nsw_psid_rdd <- read_csv('nsw_psid_rdd.csv')

#a proportion of eligible

sum(nsw_psid_rdd$re75<=1000)/nrow(nsw_psid_rdd)

#b diff in mean outcomes (-1,218.905)

mean(nsw_psid_rdd$re78[nsw_psid_rdd$re75<=1000]) - mean(nsw_psid_rdd$re78[nsw_psid_rdd$re75>1000])

#c (diff in age, black)

eligible <- nsw_psid_rdd |> filter(re75<=1000)
non_eligible <- nsw_psid_rdd |> filter(re75>1000)

nsw_psid_rdd <- nsw_psid_rdd |> mutate(eligible = (re75<=1000))

#overall
pstest0(varlist = c('age', 'black'), treated = "treated", data = nsw_psid_rdd)

#eligible
pstest0(varlist = c('age', 'black'), treated = "treated", data = eligible)

#non-eligible
pstest0(varlist = c('age', 'black'), treated = "treated", data = non_eligible)

#distributions

nsw_psid_rdd |> ggplot(aes(x = age, fill = eligible)) + geom_density()
nsw_psid_rdd |> ggplot(aes(x = 'black', y = black)) + geom_col()
