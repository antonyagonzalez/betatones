#' Extract regression model data for 3 way interaction
#'
#' Pull the regression coefficient, standard error, t-value and p-value
#' for the three way interaction specified in your linear model.
#' If you have a covariate in your model,
#' use cov_interact3 (for 2 covariates, use cov2_interact3).
#'
#' @param lm linear model
#'
#' @return beta, se, t-value, p-value
#' @export
#' @examples
#' example <- lm(formula = Sepal.Length ~ Petal.Length * Petal.Width * Sepal.Width, data = iris)
#' interact3(example)
interact3 <- function(lm) {
  modelsum <- summary(lm)
  modelci <- confint(lm, level = 0.95, method = "boot")
  out <- c(modelsum$coefficients[8],
           modelsum$coefficients[8,2],
           modelsum$coefficients[8,3],
           modelsum$coefficients[8,4],
           modelci[8],
           modelci[8,2])
  names(out) <- c("beta", "se", "t-value", "p-value", "ci95_low", "ci95_high")
  return(out)
}
