\name{expand.mgrid}
\alias{expand.mgrid}
\title{expand grid for data frames}
\usage{
  expand.mgrid(...)
}
\arguments{
  \item{...}{data frames}
}
\value{
  A data.frame with all possible combinations of the
  original data frames in \code{\link{...}}.
}
\description{
  Performs the equivalent of \code{\link{expand.grid}} on
  data frames.
}
\examples{
\donttest{
x=data.frame(value=0:1)
expand.mgrid(mtcars[1:3,c("wt","cyl")],iris[50:51,c("Petal.Width","Species")],x)
}
}
\seealso{
  \code{\link{expand.grid}}
}

