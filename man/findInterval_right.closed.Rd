\name{findInterval_right.closed}
\alias{findInterval_right.closed}
\title{Find Interval Numbers or Indices}
\usage{
  findInterval_right.closed(x, vec,
    leftmost.closed = FALSE, all.inside = FALSE)
}
\arguments{
  \item{x}{numeric.}

  \item{vec}{numeric, sorted (weakly) increasingly, of
  length \code{N}, say.}

  \item{leftmost.closed}{logical; if true, the leftmost
  interval,\code{vec[1] .. vec[2]} is treated as
  \emph{closed}, see below.}

  \item{all.inside}{logical; if true, the returned indices
  are coerced into \code{1,\dots,N-1}, i.e., \code{0} is
  mapped to \code{1} and \code{N} to \code{N-1}.}
}
\value{
  vector of length \code{length(x)} with values in
  \code{0:N} (and \code{NA}) where \code{N <- length(vec)},
  or values coerced to \code{1:(N-1)} if and only if
  \code{all.inside = TRUE} (equivalently coercing all x
  values \emph{inside} the intervals).  Note that
  \code{\link{NA}}s are propagated from \code{x}, and
  \code{\link{Inf}} values are allowed in both \code{x} and
  \code{vec}.
}
\description{
  Given a vector of non-decreasing breakpoints in
  \code{vec}, find the interval containing each element of
  \code{x}; i.e., if \code{i <-
  findInterval_right.closed(x,v)}, for each index \code{j}
  in \code{x} \eqn{v_{i_j} < x_j \le v_{i_j + 1}}{v[i[j]] <
  x[j] \le v[i[j] + 1]} where \eqn{v_0 := -\infty}{v[0] :=
  - Inf}, \eqn{v_{N+1} := +\infty}{v[N+1] := + Inf}, and
  \code{N <- length(v)}. At the two boundaries, the
  returned index may differ by 1, depending on the optional
  arguments \code{leftmost.closed} and \code{all.inside}
}
\details{
  The function \code{findInterval_right.closed} finds the
  index of one vector \code{x} in another, \code{vec},
  where the latter must be non-decreasing.  Where this is
  trivial, equivalent to \code{apply( outer(x, vec, ">"),
  1, sum)},

  When \code{leftmost.closed = TRUE}, the result for
  \code{x[j] = vec[1]} (\eqn{ = \min vec}{ = min(vec)}), is
  \code{1} as for all other values in the first interval.
}
\seealso{
  \code{\link{findInterval}}
}

