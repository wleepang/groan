\name{logi}
\alias{logi}
\title{Logistic sigmoid}
\usage{
  logi(x, k = 1)
}
\arguments{
  \item{x}{x-coordinates}

  \item{k}{exponential rate}
}
\value{
  The logistic sigmoid function with rate \code{k}
  evaluated at \code{x}
}
\description{
  \deqn{y = \frac{1}{1 + exp(-k*x)}}
}

