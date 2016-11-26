#' Meta-analyze two standardized regression coefficients
#'
#' If your regression coefficients are standardized, input the
#' coefficients and standard errors for each to compute a meta-analyzed beta
#' based off of Lipsey & Wilson (2001). P-values based off of 2-sided test.
#' Designed for meta analyzing two regression coefficients only.
#'
#' @param b1 coefficient 1
#' @param se1 standard error of coef 1
#' @param b2 coefficient 2
#' @param se2 standard error of coef 2
#'
#' @return beta, se, z-score, p-value, ci95
#' @export
#' @examples
#' example
metabeta <- function(b1, se1, b2, se2) {
  inv1 <- (1/((se1)^2))
  inv2 <- (1/((se2)^2))
  sum_inv <- (inv1 + inv2)
  weight1 <- (b1 * inv1)
  weight2 <- (b2 * inv2)
  sum_weight <- (weight1 + weight2)
  meta_b <- (sum_weight/sum_inv)
  meta_se <- sqrt(1/sum_inv)
  z <- (meta_b/meta_se)
  p <- 2*pnorm(-abs(z))
  ci95_low <- (meta_b - (1.96 * meta_se))
  ci95_high <- (meta_b + (1.96 * meta_se))
  out <- c(meta_b,
           meta_se,
           z,
           p,
           ci95_low,
           ci95_high)
  names(out) <- c("beta", "se", "z-score", "p-value", "ci95_low", "ci95_high")
  return(out)
}
