\name{match.in}
\alias{match.in}
\title{Match a vector to a list of vector}
\usage{
  match.in(x, list.table)
}
\description{
  Match a vector to a list of vector
}
\examples{
\donttest{
x=letters[1:4]
list.table=list("c","e",c("a","b"),c("b","5"))
match.in(x,list.table)
}
}
\seealso{
  \code{\link{match}}
}

