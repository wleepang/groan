\name{plot.Curves}
\alias{lines.Curves}
\alias{plot.Curves}
\alias{points.Curves}
\title{Plot Curves}
\usage{
  plot.Curves(x, hlines = NULL, vlines = NULL, lyt = NULL,
    ...)

  lines.Curves(x, lyt, ...)

  points.Curves(x, lyt, ...)
}
\arguments{
  \item{x}{A groan Curves object}

  \item{hlines}{A named list specifying arguments to
  \code{abline()} for drawing horizontal annotation lines.
  The first element should be a numeric vector of length =
  # of curves, and is assumed to be the value of the
  argument \code{h=}.  Default is NULL, no lines drawn.}

  \item{vlines}{A named list specifying arguments to
  \code{abline()} for drawing vertical annotation lines.
  The first element should be a numeric vector of length =
  # of curves, and is assumed to be the value of the
  argument \code{v=}.  Default is NULL, no lines drawn.}

  \item{lyt}{A named list specifying arguments to
  \code{layout()} that define the arrangement of curve
  thumbnails. Default is NULL, which generates a grid as
  close to a square as possible, with a bias towards a
  landscape (wide) layout.}

  \item{...}{Additional arguments passed to \code{plot()}
  for the underlying individual curve data.}
}
\value{
  The grid layout matrix so that it can be reused in other
  plotting methods.
}
\description{
  Plots a Curves object as a grid of thumbnails.  Optional
  arguments allow for horizontal and vertical annotation
  lines and grid layout.
}
\details{
  Arguments \code{hlines} and \code{vlines} are processed
  via \code{do.call()}.
}
\section{Warnings
  This function uses \code{layout()} to generate a grid of thumbnails. It is
  therefore incompatible with other device subplotting mechanims such as
  \link[graphics]{par(mfrow)}, \link[graphics]{par(mfcol)}, and
  \link[graphics]{split.screen}}{

}
\seealso{
  \link[graphics]{plot.default}, \link[graphics]{lines},
  \link[graphics]{points}, \link[graphics]{layout},
  \link[base]{do.call}
}

