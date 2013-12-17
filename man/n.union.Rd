\name{n.union}
\alias{n.union}
\title{n-ary set union}
\usage{
  n.union(...)
}
\arguments{
  \item{...}{vectors (of the same mode)}
}
\value{
  The union of all vectors.
}
\description{
  Performs set union on an unspecified number of arguments
}
\examples{
\donttest{
n.union(1:5)
n.union(1:5,61) # same as base::union
n.union(1:5,61,3:6)
}
}
\seealso{
  \code{\link{Reduce_operator}}
}

