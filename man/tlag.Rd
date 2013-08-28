\name{tlag}
\alias{groan.tlag}
\alias{tlag}
\title{Estimate lag time}
\usage{
  tlag(x, pct.thresh = 0.05)

  groan.tlag(...)
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
  Estimate lag time
}
\details{
  Scans the curve going forward in time.  The point at
  which the curve crosses the defined threshold is
  returned.
}
\seealso{
  \link[groan]{groan.tsat}
}

