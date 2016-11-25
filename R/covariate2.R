#' Extract regression model data for covariate 2
#'
#' Pull the regression coefficient, standard error, t-value and p-value
#' for the second covariate specified in your linear model.
#'
#' @param lm linear model
#'
#' @return beta, se, t-value, p-value
#' @export
#' @examples
#' example <- lm(formula = Sepal.Length ~ Petal.Length * Petal.Width * Sepal.Width, data = iris)
#' covariate2(example)
covariate2 <- function(lm) {
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
