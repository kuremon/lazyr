\name{\%within\%}
\alias{\%o_within\%}
\alias{\%o_within_o\%}
\alias{\%owo\%}
\alias{\%within\%}
\alias{\%within_o\%}
\title{Test for interval membership}
\usage{
  %within%(x, interval)

  %o_within%(x, interval)

  %within_o%(x, interval)

  %o_within_o%(x, interval)

  %owo%(x, interval)
}
\arguments{
  \item{x}{the numeric value to test.}

  \item{interval}{the interval to test.}
}
\value{
  A boolean
}
\description{
  This function tests if a \code{x} belongs to the interval
  \code{interval}.
}
\details{
  \code{\%within\%} tests for a closed interval

  \code{\%o_within\%} tests for an left-open right-close
  interval

  \code{\%within_o\%} tests for a left-close right-open
  interval

  \code{\%o_within_o\%} or \code{\%owo\%} tests for an open
  interval
}
\seealso{
  \code{\link{\%in\%}}
}

