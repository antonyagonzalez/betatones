#' Extract regression model data for 2 way interaction (predictor 2 & 3)
#'
#' Pull the regression coefficient, standard error, t-value and p-value
#' for the two way interaction between the first and third predictors
#' specified in your linear model. If you have a covariate in your model,
#' use cov_interact23 (for 2 covariates, use cov2_interact23).
#'
#' @param lm linear model
#'
#' @return beta, se, t-value, p-value
#' @export
#' @examples
#' example <- lm(formula = Sepal.Length ~ Petal.Length * Petal.Width * Sepal.Width, data = iris)
#' interact23(example)
interact23 <- function(lm) {
  modelsum <- summary(lm)
  out <- c(modelsum$coefficients[7],
           modelsum$coefficients[7,2],
           modelsum$coefficients[7,3],
           modelsum$coefficients[7,4])
  names(out) <- c("beta", "se", "t-value", "p-value")
  return(out)
}
