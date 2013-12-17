\name{see}
\alias{see}
\title{See heatmap representation of data frame}
\usage{
  see(data, use.rownames = TRUE, tilt.colnames = TRUE,
    col.as.factor = FALSE, with.text = FALSE,
    color = "black", format, ...)
}
\arguments{
  \item{data}{data to view. Can be a matrix or a data
  frame.}

  \item{use.rownames}{specify if the row names of the data
  frame should be displayed on the y axis.}

  \item{tilt.colnames}{specify if the column names should
  be tiltled on the x axis.}

  \item{col.as.factor}{specify if the values of the data
  frame should be considered as factor. By default it is
  the case if all columns of the data frame are
  categorical.}
}
\value{
  a ggplot2 object
}
\description{
  Display data frame in the plot panel with value of the
  data frame being represented as color.
}

