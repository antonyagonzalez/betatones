#' Extract regression model data for predictor 1
#'
#' Pull the regression coefficient, standard error, t-value and p-value
#' for the first predictor specified in your linear model.
#'
#' @param lm linear model
#'
#' @return beta, se, t-value, p-value
#' @export
#' @examples
#' example <- lm(formula = Sepal.Length ~ Petal.Length * Petal.Width, data = iris)
#' predictor1(example)
predictor1 <- function(lm) {
  modelsum <- summary(lm)
  modelci <- confint(lm, level = 0.95, method = "boot")
  out <- c(modelsum$coefficients[2],
           modelsum$coefficients[2,2],
           modelsum$coefficients[2,3],
           modelsum$coefficients[2,4],
           modelci[1],
           modelci[1,2])
  names(out) <- c("beta", "se", "t-value", "p-value", "ci95_low", "ci95_high")
  return(out)
}
