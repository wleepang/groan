\name{capacity}
\alias{capacity}
\alias{groan.capacity}
\title{Estimate carrying capacity}
\usage{
  capacity(x)

  groan.capacity(...)
}
\arguments{
  \item{x}{A groan Curves object of growth curves}
}
\value{
  A numeric vector of capacity values
}
\description{
  Estimate carrying capacity
}
\details{
  \code{x} is fitted to a logistic growth curve (if it has
  not been already).  The value of the fitted curve at
  \code{x-coord = Inf} is returned.
}

