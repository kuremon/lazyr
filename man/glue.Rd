\name{glue}
\alias{glue}
\title{Glue two data frames}
\usage{
  glue(x, y, by = intersect(names(x), names(y)), by.x = by,
    by.y = by, all.x = TRUE, suffixes = c(".x", ".y"))
}
\arguments{
  \item{x,}{y data frames, or objects to be coerced to
  one.}

  \item{by,}{by.x, by.y specifications of the columns used
  for merging}

  \item{all.x}{logical; if TRUE, then extra rows will be
  added to the output, one for each row in \code{x} that
  has no matching row in \code{y}. These rows will have
  \code{NA}s in those columns that are usually filled with
  values from \code{y}. The default is \code{TRUE}, so that
  all rows of \code{x} are included in the output.}

  \item{suffixes}{a character vector of length 2 specifying
  the suffixes to be used for making unique the names of
  columns in the result which not used for merging
  (appearing in \code{by}).}
}
\description{
  Merge two data frames by common columns such that the
  original layout of the first data frame is retained and
  the extra information contained in the second is added to
  the right.
}
\examples{
\donttest{
info.gear=data.frame(am=0:1,name=c("manual","automatic"),info=c("Worldwide","Mainly US"))
glue(mtcars,info.gear)
glue(mtcars,cbind(info.gear,wt="not specified"),by="am")
info.iris=data.frame(Species=c("versicolor","virginica","sibirica"),Origin=c("North America","Florida, Georgia","Russia"))
glue(iris,info.iris)
glue(iris,info.iris,all.x=FALSE)
}
}

