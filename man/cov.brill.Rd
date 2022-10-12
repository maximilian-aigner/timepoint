\name{cov.brill}
\alias{cov.brill}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Brillinger's estimator for the covariance
}
\description{
Brillinger (1976) proposed a debiased empirical estimator for the covariance. This
method implements the suggested estimator based on histogram binning and optionally plots the resulting estimated covariance.
}
\usage{
cov.brill(eventlist, bw, debias = TRUE, plot = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eventlist}{
An \code{eventlist} object
}
  \item{bw}{
Histogram bin width, default is 100 divided by the interval length.
}
  \item{debias}{
Apply a debiasing step.
}
  \item{plot}{
Plot the resulting covariance.
}
  \item{...}{
  Extra arguments to \code{plot}
}
}
\value{
The covariance estimate
}
\references{
Brillinger, David R. 'Estimation of the Second-Order Intensities of a Bivariate Stationary Point Process'. Journal of the Royal Statistical Society. Series B (Methodological) 38, no. 1 (1976): 60–66.
}
\author{
Maximilian Aigner
}
\seealso{
\code{spec.pp}
}
