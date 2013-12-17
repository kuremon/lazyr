\name{insert}
\alias{insert}
\title{Insert values within a vector at specified positions}
\usage{
  insert(v, values, index)
}
\arguments{
  \item{v}{a vector.}

  \item{values}{vector to add to \code{v}.}

  \item{index}{position index vector the same length of
  \code{values}.}
}
\value{
  The output vector \code{v.res} is such that
  \code{v.res[index]=values} and \code{v.res[-index]=v}.
}
\description{
  \code{insert} inserts values within a vector at specified
  position.
}

