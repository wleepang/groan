\name{logip}
\alias{logip}
\title{Logistic sigmoid (parameterized)}
\usage{
  logip(x, p = c(d = 0, h = 1, k = 1, y0 = 0))
}
\arguments{
  \item{x}{x-coordinates}

  \item{p}{named vector of curve parameters: \describe{
  \item{y0}{y-offset (minimum asymptote)} \item{d}{delay}
  \item{h}{maximum asymptote} \item{k}{climb rate} }}
}
\value{
  The logistic function specified by \code{p} evaluated at
  \code{x}
}
\description{
  \deqn{y = y0 + (h-y0)*\frac{1}{1 + exp(-k*(x-d))}}
}
\seealso{
  \link[groan]{logi}
}

