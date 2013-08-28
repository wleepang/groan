\name{tsat}
\alias{groan.tsat}
\alias{tsat}
\title{Estimate saturation time}
\usage{
  tsat(x, pct.thresh = 0.05)

  groan.tsat(...)
}
\arguments{
  \item{x}{A groan Curves object of specific growth rate
  profiles}

  \item{pct.thresh}{Percent threshold relative to the curve
  maximum for detection.  Default is 5\% (0.05).}
}
\value{
  A numeric vector of lag times
}
\description{
  Estimate saturation time
}
\details{
  Scans the curve going backward in time.  The point at
  which the curve crosses the defined threshold is
  returned.
}
\seealso{
  \link[groan]{groan.tlag}
}

