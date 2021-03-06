\name{generations}
\alias{generations}
\alias{groan.generations}
\title{Calculate number of generations}
\usage{
  generations(x)

  groan.generations(...)
}
\arguments{
  \item{x}{A groan Curves object representing specific
  growth rates}
}
\value{
  An atomic numeric vector
}
\description{
  Calculates the number of generations in a growth curve by
  computing the area under the specific rate profile
}
\seealso{
  \link[stats]{splinefun}, \link[stats]{integrate}
}

