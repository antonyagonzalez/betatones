% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cov_interact23.R
\name{cov_interact23}
\alias{cov_interact23}
\title{Extract regression model data for 2 way interaction (predictor 2 & 3)
with one covariate}
\usage{
cov_interact23(lm)
}
\arguments{
\item{lm}{linear model}
}
\value{
beta, se, t-value, p-value
}
\description{
Pull the regression coefficient, standard error, t-value and p-value
for the two way interaction between the second and third predictors
specified in your linear model. If you have no covariates in your model,
use interact23 (for 2 covariates, use cov2_interact23).
}
\examples{
example <- lm(formula = Sepal.Length ~ Petal.Length * Petal.Width * Sepal.Width, data = iris)
cov_interact23(example)
}

