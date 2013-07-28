\name{groan.logifit}
\alias{groan.logifit}
\title{Logistic sigmoid model fitting}
\usage{
  groan.logifit(x)
}
\arguments{
  \item{x}{A groan Curves object}
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
  \link[groan]{groan.pulsefit}, \link[stats]{nlminb}
}

