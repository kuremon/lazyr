\name{rows.as.list}
\alias{rows.as.list}
\title{Coerce a data frame to the list of its row}
\usage{
  rows.as.list(data)
}
\arguments{
  \item{data}{the data frame to coerce}
}
\value{
  A list of all the rows (data frame themselves) of
  \code{data}.
}
\description{
  Coerce a data frame to the list of its row
}
\examples{
\donttest{
rows.as.list(mtcars)
}
}

