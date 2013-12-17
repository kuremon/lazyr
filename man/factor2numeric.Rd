\name{factor2numeric}
\alias{factor2numeric}
\title{Coerce to the numeric value contained in the object}
\usage{
  factor2numeric(x)
}
\arguments{
  \item{x}{the object to coerce to numeric.}
}
\value{
  A numeric vector.
}
\description{
  Coerce to the numeric value contained in the object
}
\details{
  This function is equivalent to \code{as.numeric} except
  for factor where it returns the contained numeric value,
  not the order of the levels.
}

