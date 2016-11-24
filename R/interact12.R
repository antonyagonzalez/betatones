#' Extract regression model data for 2 way interaction (predictor 1 & 2)
#'
#' Pull the regression coefficient, standard error, t-value and p-value
#' for the two way interaction between the first and second predictors
#' specified in your linear model. If you have a covariate in your model,
#' use cov_interact12 (for 2 covariates, use cov2_interact12).
#'
#' @param lm linear model
#'
#' @return beta, se, t-value, p-value
#' @export
#' @examples
#' example <- lm(formula = Sepal.Length ~ Petal.Length * Petal.Width * Sepal.Width, data = iris)
#' interact12(example)
interact12 <- function(lm) {
  modelsum <- summary(lm)
  out <- c(modelsum$coefficients[5],
           modelsum$coefficients[5,2],
           modelsum$coefficients[5,3],
           modelsum$coefficients[5,4])
  names(out) <- c("beta", "se", "t-value", "p-value")
  return(out)
}
