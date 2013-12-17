\name{insert.row}
\alias{insert.row}
\title{Insert rows in a data frame}
\usage{
  insert.row(data, new.rows, index = NULL)
}
\arguments{
  \item{data}{the data.frame to update}

  \item{new.rows}{the rows (data frame) to add (must match
  the format of \code{data}).}

  \item{index}{the indexes at which the new rows should be
  added. By default the new rows are added at the end of
  the \code{data}.}
}
\value{
  The updated data frame.
}
\description{
  Insert one or more rows in a data frame
}
\details{
  For values of \code{index} strictly negative, the
  corresponding \code{new.rows} are added at the
  \code{abs(where)} positions before the end of
  \code{data}. For example, for a single row, if
  \code{index=-1}, the new row is added at the penultimate
  position of the existing data frame.
}
\seealso{
  \code{\link{insert}}
}

