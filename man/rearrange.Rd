\name{rearrange}
\alias{rearrange}
\title{Rearrange order}
\usage{
  rearrange(data, match, ...)
}
\arguments{
  \item{data}{data to rearrange (can be a vector or a data
  frame).}

  \item{match}{argument specifying the name of the values
  (columns or atomic values) to move and the new indexes.}
}
\description{
  This is the generic rearrange function. See the following
  functions for the details about different data
  structures.
}
\details{
  \itemize{ \item \code{\link{rearrange.default}} when
  \code{match} is non-specified or does not fall in any
  classes listed below.  \item
  \code{\link{rearrange.numeric}} when \code{match} is a
  named numeric vector.  \item \code{\link{rearrange.list}}
  when \code{match} is a named numeric list.  \item
  \code{\link{rearrange.data.frame}} when \code{match} is a
  data frame. } The arguments \code{from} and \code{to} are
  ignored if \code{match} is specified.
}
\seealso{
  \code{\link{insert}}
}

