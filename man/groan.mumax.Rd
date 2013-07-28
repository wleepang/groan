\name{groan.mumax}
\alias{groan.mumax}
\title{Maximum specific growth rate}
\usage{
  groan.mumax(x, na.rm = T)
}
\arguments{
  \item{x}{A groan Curves object of specific growth rate
  profiles}

  \item{na.rm}{Boolean, specifies if NA values are removed
  when finding max rates.}
}
\value{
  A numeric vector of maximum specific growth rates
}
\description{
  Maximum specific growth rate
}
\details{
  Simply a wrapper for \code{vapply}-ing \code{max} over
  the Curves object.
}

