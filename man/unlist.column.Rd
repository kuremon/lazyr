\name{unlist.column}
\alias{unlist.column}
\title{Unlist a column of a data frame}
\usage{
  unlist.column(data, j)
}
\arguments{
  \item{data}{the data frame}

  \item{j}{the column name or index}
}
\description{
  This function unlists the column of a data frame
  (typically the column to unlist will be a list of lists
  or a list of vectors) while retaining information about
  the other columns. This function can be usefull when
  working with survey data with multiple answers. By
  unlisting the answers, one can look at occurence,
  repartition, etc of class of individual answers.
}
\examples{
\donttest{
survey.data=data.frame(id=1:3,believe.in.god=c(T,T,F))
survey.data$why=list("faith",c("faith","fear of hell"),"R is my god")
unlist.column(survey.data,"why")
}
}

