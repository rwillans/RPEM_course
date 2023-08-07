library(tidyverse)
library(MatchIt)
library(cobalt)

source('pstest0.R')
source('pstest_scatter.R')

# Load in (original) NSW dataset

nsw <- read_csv("nsw.csv")

glimpse(nsw)

#1 Check random assignment - looks ok

var_diffs <- MatchIt::matchit(treated ~ age + age2 + educ + black + hispanic + married + nodegree + re75,
                     data = nsw)

pstest0(varlist = names(nsw)[2:9], treated = "treated", data = nsw)

love.plot(var_diffs, stars = "std")

pstest0(varlist = names(nsw)[2:9], treated = "treated", data = nsw) |> pstest_scatter(xmax = 25)

#2 Derive the benchmark

lm(re78 ~ treated, data = nsw) |> summary()

# Effect is additional 886.3 (though only significant at 10% level)

# Add in other controls ('mop up') - effect is now 798

lm(re78 ~ treated + age + age2 + educ + black + hispanic + nodegree, data = nsw) |> summary()

# respecify only including black as covariant

lm(re78 ~ treated + black + educ, data = nsw) |> summary()

#3 Explore hetrogeneity in impacts

#d mean effect split by age

#over 24 treatment effect is 1593.4
lm(re78 ~ treated, data = (nsw |> filter(age>24))) |> summary()
                              
#24 and under treatment effect is 343
lm(re78 ~ treated, data = (nsw |> filter(age<25))) |> summary()
