\name{d}
\alias{d}
\title{Create a Date object}
\usage{
  d(days, months, years, expand = F)
}
\arguments{
  \item{days}{vector of days. Can be code{numeric} or
  \code{character}}

  \item{months}{vector of months.}

  \item{years}{the vector of years.}

  \item{expand}{a boolean (see Details section).}
}
\value{
  A vector of object of class \code{Date}.
}
\description{
  Create date objects with an easy syntax
}
\details{
  If \code{expand} is \code{TRUE}, then all the combanition
  of dates from the input vectors are creator. If
  \code{expand} is \code{FALSE}, then the smaller vectors
  are replicated to match the length of the longest vector
  (\code{N}) and \code{N} dates are created such that the
  i-th date is \code{d(days[i],months[i],years[i])}.
}
\seealso{
  \code{\link{is.integer}}
}

