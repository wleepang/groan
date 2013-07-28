\name{groan.pulsefit}
\alias{groan.pulsefit}
\title{Pulse model fitting}
\usage{
  groan.pulsefit(x)
}
\arguments{
  \item{x}{A groan Curves object}
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
  \link[groan]{groan.logifit}, \link[stats]{nlminb}
}

