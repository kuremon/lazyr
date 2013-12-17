#' @title Is a numeric value an integer? 
#' @description 
#' Test if a numeric object (or integer) contains an integer. \code{is.numeric} only tests the class of the object such that 
#' \code{is.numeric(1)} returns \code{FALSE}.
#' @param x the object to test.
#' @return A boolean
#' @seealso \code{\link{is.integer}}
#' @export
contains.integer<-function(x){
  return(x%%1==0);
}

#' @title Check if the object is categorical (that is not numeric)
#' @description 
#' Test if an object belongs to class factor, character or logical. These three types are defined as categorical.
#' @param x the object to test.
#' @return A boolean
#' @seealso \code{\link{contains.integer}}
#' @export
is.categorical<-function(x){
  return(inherits(x,c("factor","character","logical")));
}

#' @title Coerce to the numeric value contained in the object 
#' @param x the object to coerce to numeric.
#' @return A numeric vector.
#' @details
#' This function is equivalent to \code{as.numeric} except for factor where it returns the contained numeric value, not the order
#' of the levels.
#' @export
factor2numeric <- function(x){
  return(as.numeric(as.character(x)));
}

#' @title Coerce a data frame to the list of its row
#' @param data the data frame to coerce
#' @return A list of all the rows (data frame themselves) of \code{data}.
#' @export
#' @examples
#' \donttest{
#' rows.as.list(mtcars)
#' }
rows.as.list=function(data){
  alply(data,1,identity)
}

#' @title Find Interval Numbers or Indices
#' @description 
#' Given a vector of non-decreasing breakpoints in \code{vec}, find the interval containing each element of \code{x}; i.e., if 
#' \code{i <- findInterval_right.closed(x,v)}, for each index \code{j} in \code{x} 
#' \eqn{v_{i_j} < x_j \le v_{i_j + 1}}{v[i[j]] < x[j] \le v[i[j] + 1]} where \eqn{v_0 := -\infty}{v[0] := - Inf}, 
#' \eqn{v_{N+1} := +\infty}{v[N+1] := + Inf}, and \code{N <- length(v)}.
#' At the two boundaries, the returned index may differ by 1, depending on the optional arguments \code{leftmost.closed} and \code{all.inside}
#' @param x numeric.
#' @param vec numeric, sorted (weakly) increasingly, of length \code{N}, say.
#' @param leftmost.closed logical; if true, the leftmost interval,\code{vec[1] .. vec[2]} is treated as \emph{closed}, see below.
#' @param all.inside logical; if true, the returned indices are coerced
#' into \code{1,\dots,N-1}, i.e., \code{0} is mapped to \code{1}
#' and \code{N} to \code{N-1}.
#' @details
#'  The function \code{findInterval_right.closed} finds the index of one vector \code{x} in 
#'  another, \code{vec}, where the latter must be non-decreasing.  Where
#'  this is trivial, equivalent to \code{apply( outer(x, vec, ">"), 1, sum)},
#'  
#'  When \code{leftmost.closed = TRUE}, the result for \code{x[j] = vec[1]}
#'  (\eqn{ = \min vec}{ = min(vec)}), is \code{1} as for all other
#'  values in the first interval.
#' @return vector of length \code{length(x)} with values in \code{0:N} (and
#' \code{NA}) where \code{N <- length(vec)}, or values coerced to
#' \code{1:(N-1)} if and only if \code{all.inside = TRUE} (equivalently coercing all 
#' x values \emph{inside} the intervals).  Note that \code{\link{NA}}s are
#' propagated from \code{x}, and \code{\link{Inf}} values are allowed in
#' both \code{x} and \code{vec}.
#' @seealso \code{\link{findInterval}}
#' @export
findInterval_right.closed<-function(x, vec, leftmost.closed = FALSE, all.inside = FALSE){
  if(any(diff(vec)<0)) stop("'vec' must be sorted non-decreasingly");
  if(all.inside){
    vec[1]=-Inf;
    vec[length(vec)]=Inf;
  }
  index=apply(outer(x,vec,">"),1,sum);
  if(leftmost.closed)index[x==vec[1]]=1;
  return(index);
}

#' @title Returns the start and end of the TRUE sequences
#' @export
get_intervals=function(b){
 
  diff.b=diff(as.numeric(b));
  index=which(diff.b!=0);
  
  if(length(index)==0){
    if(b[1]){
      return(data.frame(first=1,last=length(b)))
    }
    else{
      return(NULL);
    }
  }
  
  if(diff.b[index[1]]==-1){
    index=c(0,index);
  }
  if(diff.b[tail(index,1)]==1){
    index=c(index,length(b));
  }
  
  intervals=matrix(index,ncol=2,byrow=T);
  intervals[,1]=intervals[,1]+1;
  colnames(intervals)=c("first","last");
  return(as.data.frame(intervals))
}

#' @title Unlist a column of a data frame
#' @description This function unlists the column of a data frame (typically the column to unlist
#' will be a list of lists or a list of vectors) while retaining information about the other
#' columns. This function can be usefull when working with survey data with multiple answers.
#' By unlisting the answers, one can look at occurence, repartition, etc of class of individual
#' answers. 
#' @param data the data frame
#' @param j the column name or index
#' @export
#' @examples
#' \donttest{
#' survey.data=data.frame(id=1:3,believe.in.god=c(T,T,F))
#' survey.data$why=list("faith",c("faith","fear of hell"),"R is my god")
#' unlist.column(survey.data,"why")
#' }
unlist.column=function(data,j){
  names.data=names(data)
  if(is.numeric(j)) j=names.data[j]
  
  j.unlist.name=paste0(j,".unlist")
  
  data=adply(data,1,function(df)data.frame(unlist(df[[j]])))
  data=data[names(data)!=j]
  names(data)[length(data)]=j
  data=data[names.data]
  data
}

#' @title Add to each element of a list
#' @param L A list
#' @param to.add element to add to the list
#' @export
add.list=function(L,to.add){
  lapply(L,function(x)x+to.add)
}