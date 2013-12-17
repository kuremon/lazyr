\name{rearrange.numeric}
\alias{rearrange.numeric}
\title{Rearrange order in data when names are specified.}
\usage{
  \method{rearrange}{numeric} (data, match, ...)
}
\arguments{
  \item{data}{data to rearrange.}

  \item{match}{a named vector, list or data frame giving
  the new position of the columns of \code{data}. If a name
  is specified but does not correspond to a variable in
  \code{data}, the corresponding transformation is
  ignored.}
}
\value{
  The re-arranged data \code{data.res} of length
  \code{length(v)}. \code{v.res} is such that
  \code{v.res[new.index]=v[current.index]} and
  \code{v.res[-new.index]=v[-current.index]}.
}
\description{
  \code{rearrange} moves part of data to specified
  positions.
}
\seealso{
  \code{\link{insert}}
}

