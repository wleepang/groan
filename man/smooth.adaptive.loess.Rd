\name{smooth.adaptive.loess}
\alias{smooth.adaptive.loess}
\title{Adaptive smoothing using optimized loess}
\usage{
  smooth.adaptive.loess(x, y = NULL,
    span.interval = c(0.1, 0.7))
}
\arguments{
  \item{x}{An \code{xy.coords} object, \code{list()} with
  named numeric elements \code{x} and \code{y}, or an
  atomic numeric vector of x-coordinates.  If an atomic
  vector the parameter \code{y} is required.}

  \item{y}{Required if the parameter \code{x} is specified
  as an atomic vector.  An atomic vector of y-coordinates.}

  \item{span.interval}{The interval of \code{span} values,
  used by \link[stats]{loess}, over which to
  \link[stats]{optimize} the fit. The optimization
  minimizes the value of (sum of squared residuals)/span.}
}
\value{
  A list with components \describe{ \item{x}{x-coordinates
  of the smoothed curve} \item{y}{y-coordinates of the
  smoothed curve} \item{span}{the \code{span} value used
  for the optimized fit} \item{objective}{the final value
  of the objective function} }
}
\description{
  Performs loess smoothing with adaptive parameter
  optimization
}
\seealso{
  \link[stats]{loess} \link[stats]{optimize}
}

