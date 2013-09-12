\name{groan.fit.logi}
\alias{groan.fit.logi}
\title{Logistic sigmoid model fitting}
\usage{
  groan.fit.logi(x, FUN.PROGRESS = NULL, ...)
}
\arguments{
  \item{x}{A groan Curves object}

  \item{FUN.PROGRESS}{Function to call after each fitting
  operation to indicate progress.  Default is \code{NULL},
  nothing is done.}
}
\value{
  A groan Curves object where y-coordinates are the fitted
  logistic sigmoid profile.  In addition, results of the
  fit, e.g. sigmoid parameters and goodness-of-fit
  statistics.
}
\description{
  Fits curves (e.g. growth curves) to a logisitic sigmoid.
}
\seealso{
  \link[groan]{groan.fit.pulse}, \link[stats]{nlminb}
}

