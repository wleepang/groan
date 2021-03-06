\name{groan.fit.pulse}
\alias{groan.fit.pulse}
\title{Pulse model fitting}
\usage{
  groan.fit.pulse(x, FUN.PROGRESS = NULL, ...)
}
\arguments{
  \item{x}{A groan Curves object}

  \item{FUN.PROGRESS}{Function to call after each fitting
  operation to indicate progress.  Default is \code{NULL},
  nothing is done.}
}
\value{
  A groan Curves object where y-coordinates are the fitted
  pulse profile.  In addition, results of the fit, e.g.
  pulse parameters and goodness-of-fit statistics.
}
\description{
  Fits curves (e.g. specific rate profiles) to a pulse
  derived from a mixture of logistic sigmoids
}
\seealso{
  \link[groan]{groan.fit.logi}, \link[stats]{nlminb}
}

