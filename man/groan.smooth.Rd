\name{groan.smooth}
\alias{groan.smooth}
\title{Apply smoothing to a groan Curves object}
\usage{
  groan.smooth(x, method = c("spline", "loess", "lowess"),
    adaptive = FALSE,
    FUN.IN = list(x = identity, y = identity),
    FUN.OUT = list(x = identity, y = identity), ...)
}
\arguments{
  \item{x}{A groan Curves object}

  \item{method}{Underlying smoothing algorithm to apply.}

  \item{adaptive}{Defines if an adaptive approach should be
  applied. See details.}

  \item{FUN.IN}{List of functions to apply to
  x,y-coordinates prior to smoothing.  Default is
  \code{list(x=identity, y=identity)}.}

  \item{FUN.OUT}{List of function to apply to
  x,y-coordinates post smoothing.  Typically, this is the
  inverse of the function applied in \code{FUN.IN}.
  Default is \code{list(x=identity, y=identity)}.}

  \item{...}{further arguments passed to underlying
  function calls.}
}
\value{
  A groan Curves object of same size as the input where all
  y-values are replaced with smoothed values.  Details of
  the smoothing process are appended as attributes.
}
\description{
  Apply smoothing to a groan Curves object
}
\details{
  Adaptive smoothing uses different approaches depending on
  the method chosen: \describe{ \item{spline}{
  \code{smooth.spline()} is run for several iterations over
  a random subset of the input data (aka jack-knifing).
  The final smoothed curve is computed as the average of
  all iterations (aka bootstrapped aggregation - or
  bagging).  } \item{loess, lowess}{ The span interval for
  \code{loess} and \code{lowess}, the window over which
  locally weighted polynomial fits are performed, is
  optimized using \code{optim} to minimize the sum of
  squared residuals between the smoothed curve and data.  }
  }

  Note, non-adaptive \code{loess} is wrapped so that its
  call is compatible with other functions in the set - e.g.
  it uses an \code{xy.coords} object as its first argument.
}
\seealso{
  Core smoothing methods: \link[stats]{smooth.spline},
  \link[stats]{loess}, \link[stats]{lowess}; Adaptive
  variations of the former:
  \link[groan]{smooth.adaptive.spline},
  \link[groan]{smooth.adaptive.loess},
  \link[groan]{smooth.adaptive.lowess} Also of interest for
  adaptive loess and lowess: \link[stats]{optim}
}

