\name{rearrange.default}
\alias{rearrange.default}
\title{Rearrange order in data when names are specified.}
\usage{
  \method{rearrange}{default} (data, match, from, to)
}
\arguments{
  \item{data}{data to rearrange.}

  \item{from}{name of the values to move.}

  \item{to}{indexes where the values \code{data[from]}
  should be moved to.}
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
\details{
  NA values in \code{from} or \code{to} are ignored.
}
\seealso{
  \code{\link{insert}}
}

