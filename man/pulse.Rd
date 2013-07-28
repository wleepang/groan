\name{pulse}
\alias{pulse}
\title{Logistic pulse}
\usage{
  pulse(x, d = 0, w = diff(range(x))/2, h = 1, kr = 1,
    kf = 1, p = NULL)
}
\arguments{
  \item{x}{Vector of x-coordinates}

  \item{d}{Delay (x value for pulse rise)}

  \item{w}{Pulse width. The value of \code{d+w} is the (x
  value for pulse fall)}

  \item{h}{Pulse height}

  \item{kr}{Rate of rise}

  \item{Kf}{Rate of fall}

  \item{p}{Optional. Vector of parameters where element
  names match argument names described above.  If provided,
  overides individual argument specifications.}
}
\value{
  A vector of y values for the pulse.
}
\description{
  Logistic pulse
}
\details{
  Produces a pulse based on the superposition of two
  logistic sigmoids.  \deqn{y = h*(logi(x-d, kr) -
  logi(x-(d+w), kf))}
}
\seealso{
  \link[groan]{logi}
}

