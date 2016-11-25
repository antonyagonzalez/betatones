#' Extract regression model data for 3 way interaction
#' with two covariates
#'
#' Pull the regression coefficient, standard error, t-value and p-value
#' for the three way interaction specified in your linear model.
#'
#' @param lm linear model
#'
#' @return beta, se, t-value, p-value
#' @export
#' @examples
#' example <- lm(formula = Sepal.Length ~ Petal.Length * Petal.Width * Sepal.Width, data = iris)
#' cov2_interact3(example)
cov2_interact3 <- function(lm) {
  modelsum <- summary(lm)
  modelci <- confint(lm, level = 0.95, method = "boot")
  out <- c(modelsum$coefficients[10],
           modelsum$coefficients[10,2],
           modelsum$coefficients[10,3],
           modelsum$coefficients[10,4],
           modelci[9],
           modelci[9,2])
  names(out) <- c("beta", "se", "t-value", "p-value", "ci95_low", "ci95_high")
  return(out)
}
