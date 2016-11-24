
predictor1 <- function(lm) {
  modelsum <- summary(lm)
  out <- c(modelsum$coefficients[2],
           modelsum$coefficients[2,2],
           modelsum$coefficients[2,3],
           modelsum$coefficients[2,4])
  names(out) <- c("beta", "se", "t-value", "p-value")
  return(out)
}
