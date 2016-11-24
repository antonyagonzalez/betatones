#' Extract regression model data for predictor 3
#'
#' Pull the regression coefficient, standard error, t-value and p-value
#' for the third predictor specified in your linear model.
#'
#' @param lm linear model
#'
#' @return beta, se, t-value, p-value
#' @export
#' @examples
#' example <- lm(formula = Sepal.Length ~ Petal.Length * Petal.Width * Sepal.Width, data = iris)
#' predictor3(example)
predictor3 <- function(lm) {
  modelsum <- summary(lm)
  out <- c(modelsum$coefficients[4],
           modelsum$coefficients[4,2],
           modelsum$coefficients[4,3],
           modelsum$coefficients[4,4])
  names(out) <- c("beta", "se", "t-value", "p-value")
  return(out)
}
