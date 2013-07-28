\name{groan.init}
\alias{groan.init}
\title{Initialize a groan Curves object}
\usage{
  groan.init(x)
}
\arguments{
  \item{x}{input object either a matrix or data.frame.}
}
\value{
  A groan Curves object (S3) which is simply a list of
  xy.coords objects.  Names of the list are the same as
  column names in the input \code{x}
}
\description{
  Initializes a groan Curves object from a matrix or a
  data.frame.
}
\details{
  The first column of the \code{x} is taken to be x-values
  for all curves in the data.
}
\seealso{
  \link[grDevices]{xy.coords}
}

