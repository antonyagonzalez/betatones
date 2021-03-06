% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cov_interact3.R
\name{cov_interact3}
\alias{cov_interact3}
\title{Extract regression model data for 3 way interaction
with one covariate}
\usage{
cov_interact3(lm)
}
\arguments{
\item{lm}{linear model}
}
\value{
beta, se, t-value, p-value
}
\description{
Pull the regression coefficient, standard error, t-value and p-value
for the three way interaction specified in your linear model.
If you have no covariates in your model,
use interact3 (for 2 covariates, use cov2_interact3).
}
\examples{
example <- lm(formula = Sepal.Length ~ Petal.Length * Petal.Width * Sepal.Width, data = iris)
cov_interact3(example)
}

