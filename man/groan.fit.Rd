\name{groan.fit}
\alias{groan.fit}
\title{Fit model}
\usage{
  groan.fit(x, method = c("pulse", "logi"))
}
\arguments{
  \item{x}{A groan Curves object}

  \item{model}{Model to fit to curves either: \describe{
  \item{pulse}{logistic pulse, typical for specific rate
  curves.} \item{logi}{logisic sigmoid, typical for growth
  curves.} }}
}
\value{
  A groan Curves object were y-values are replaced with
  fitted model predictions.  Also, model fit details are
  appended as additional properties.
}
\description{
  Fit model
}
\seealso{
  \link[groan]{groan.pulsefit} \link[groan]{groan.logifit}
}

