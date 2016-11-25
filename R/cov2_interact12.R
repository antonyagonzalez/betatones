#' Extract regression model data for 2 way interaction (predictor 1 & 2)
#' with two covariates
#'
#' Pull the regression coefficient, standard error, t-value and p-value
#' for the two way interaction between the first and second predictors
#' specified in your linear model.
#'
#' @param lm linear model
#'
#' @return beta, se, t-value, p-value
#' @export
#' @examples
#' example <- lm(formula = Sepal.Length ~ Petal.Length * Petal.Width * Sepal.Width, data = iris)
#' cov2_interact12(example)
cov2_interact12 <- function(lm) {
  modelsum <- summary(lm)
  modelci <- confint(lm, level = 0.95, method = "boot")
  out <- c(modelsum$coefficients[7],
           modelsum$coefficients[7,2],
           modelsum$coefficients[7,3],
           modelsum$coefficients[7,4],
           modelci[7],
           modelci[7,2])
  names(out) <- c("beta", "se", "t-value", "p-value", "ci95_low", "ci95_high")
  return(out)
}
