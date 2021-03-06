% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predictor3.R
\name{predictor3}
\alias{predictor3}
\title{Extract regression model data for predictor 3}
\usage{
predictor3(lm)
}
\arguments{
\item{lm}{linear model}
}
\value{
beta, se, t-value, p-value
}
\description{
Pull the regression coefficient, standard error, t-value and p-value
for the third predictor specified in your linear model.
}
\examples{
example <- lm(formula = Sepal.Length ~ Petal.Length * Petal.Width * Sepal.Width, data = iris)
predictor3(example)
}

