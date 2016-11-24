#' Extract regression model data for predictor 2
#'
#' Pull the regression coefficient, standard error, t-value and p-value
#' for the second predictor specified in your linear model.
#'
#' @param lm linear model
#'
#' @return beta, se, t-value, p-value
#' @export
#' @examples
#' example <- lm(formula = Sepal.Length ~ Petal.Length * Petal.Width, data = iris)
#' predictor2(example)
predictor2 <- function(lm) {
  modelsum <- summary(lm)
  out <- c(modelsum$coefficients[3],
           modelsum$coefficients[3,2],
           modelsum$coefficients[3,3],
           modelsum$coefficients[3,4])
  names(out) <- c("beta", "se", "t-value", "p-value")
  return(out)
}
