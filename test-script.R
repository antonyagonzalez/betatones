##extracting betas

##x = predictor of interest
##y = outcome variable
##var1 = other predictor 1
##lvl1 = level of predictor 1 (whichever variable = 0)
##var2 = other predictor 2
##lvl2 = level of predictor 2(whichever variable = 1)
##cov = covariate

##may want to import haven

library(devtools)
library(tidyverse)
Study1 <- read_csv("test-data.csv")

model <- lm(formula = percentcorrectZ ~ mathscoreZ * condition1 * gender1, data = Study1)
summary(model)
