#' Extract regression model data for 2 way interaction (predictor 1 & 3)
#' with two covariates
#'
#' Pull the regression coefficient, standard error, t-value and p-value
#' for the two way interaction between the first and third predictors
#' specified in your linear model.
#'
#' @param lm linear model
#'
#' @return beta, se, t-value, p-value
#' @export
#' @examples
#' example <- lm(formula = Sepal.Length ~ Petal.Length * Petal.Width * Sepal.Width, data = iris)
#' cov2_interact13(example)
cov2_interact13 <- function(lm) {
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
