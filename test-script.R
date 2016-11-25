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

predictor1 <- function(lm) {
  modelsum <- summary(lm)
  modelci <- confint(lm, level = 0.95, method = "boot")
  out <- c(modelsum$coefficients[2],
           modelsum$coefficients[2,2],
           modelsum$coefficients[2,3],
           modelsum$coefficients[2,4],
           modelci[1],
           modelci[1,2])
  names(out) <- c("beta", "se", "t-value", "p-value", "ci95_low", "ci95_high")
  return(out)
}
predictorci(model)
