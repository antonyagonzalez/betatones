#' Extract regression model data for 2 way interaction (predictor 1 & 3)
#'
#' Pull the regression coefficient, standard error, t-value and p-value
#' for the two way interaction between the first and third predictors
#' specified in your linear model. If you have a covariate in your model,
#' use cov_interact13 (for 2 covariates, use cov2_interact13).
#'
#' @param lm linear model
#'
#' @return beta, se, t-value, p-value
#' @export
#' @examples
#' example <- lm(formula = Sepal.Length ~ Petal.Length * Petal.Width * Sepal.Width, data = iris)
#' interact13(example)
interact13 <- function(lm) {
  modelsum <- summary(lm)
  modelci <- confint(lm, level = 0.95, method = "boot")
  out <- c(modelsum$coefficients[6],
           modelsum$coefficients[6,2],
           modelsum$coefficients[6,3],
           modelsum$coefficients[6,4],
           modelci[5],
           modelci[5,2])
  names(out) <- c("beta", "se", "t-value", "p-value", "ci95_low", "ci95_high")
  return(out)
}
