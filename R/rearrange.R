#### ADD 0 for row position in rearrange and NULL for supress.

rearrange_base=function(data,from,to){
  if(missing(from)|missing(to)) stop("rearrange.default: 'from' and 'to' need to be specified.")
  if(length(from)!=length(to)) stop("rearrange.default: 'from' and 'to' must have the same length.")
  na.idx=is.na(from)|is.na(to)
  if(sum(na.idx)==length(from)) return(data)
  from=from[!na.idx]
  to=to[!na.idx]
  index=seq_along(data)
  index.res=insert(v=index[-from],values=index[from],index=to)
  data[index.res]
}

#' @title Rearrange order
#' @description 
#' This is the generic rearrange function. See the following functions for the details about different data structures.  
#' @param data data to rearrange (can be a vector or a data frame).
#' @param match argument specifying the name of the values (columns or atomic values) to move and the new indexes.
#' @details
#' \itemize{
#'   \item \code{\link{rearrange.default}} when \code{match} is non-specified or does not fall in any classes listed below.
#'   \item \code{\link{rearrange.numeric}} when \code{match} is a named numeric vector.
#'   \item \code{\link{rearrange.list}} when \code{match} is a named numeric list.
#'   \item \code{\link{rearrange.data.frame}} when \code{match} is a data frame.
#' }
#' The arguments \code{from} and \code{to} are ignored if \code{match} is specified.
#' @seealso \code{\link{insert}}
#' @export
rearrange <- function(data,match,...){
  if(missing(data)) stop("argument 'data' is missing, with no default")
  if(missing(match)) return(rearrange_base(data,...))
  if(length(list(...))) warning("Only argument 'match' is taken into account")
  UseMethod("rearrange",match)
}

#' @title Rearrange order in data when names are specified.
#' @description 
#' \code{rearrange} moves part of data to specified positions. 
#' @param data data to rearrange.
#' @param from name of the values to move.
#' @param to indexes where the values \code{data[from]} should be moved to.
#' @details NA values in \code{from} or \code{to} are ignored. 
#' @return The re-arranged data \code{data.res} of length \code{length(v)}. \code{v.res}
#' is such that \code{v.res[new.index]=v[current.index]} and \code{v.res[-new.index]=v[-current.index]}.
#' @seealso \code{\link{insert}}
#' @S3method rearrange default
#' @method rearrange default
rearrange.default=function(data,match,from,to){
  warning("match is specified but does not have an associated method. rearrange.default used instead.")
  data.res=rearrange_base(data,from=from,to=to)
  return(data.res)
}


#' @title Rearrange order in data when names are specified.
#' @description 
#' \code{rearrange} moves part of data to specified positions. 
#' @param data data to rearrange.
#' @param match a named vector, list or data frame giving the new position of the columns
#' of \code{data}. If a name is specified but does not correspond to a variable in \code{data},
#' the corresponding transformation is ignored.
#' @return The re-arranged data \code{data.res} of length \code{length(v)}. \code{v.res}
#' is such that \code{v.res[new.index]=v[current.index]} and \code{v.res[-new.index]=v[-current.index]}.
#' @seealso \code{\link{insert}}
#' @S3method rearrange numeric
#' @method rearrange numeric
rearrange.numeric=function(data,match,...){
  from=base::match(names(match),names(data))
  to=match
  data.res=rearrange_base(data,from=from,to=to)
  return(data.res)
}

#' @S3method rearrange list
#' @method rearrange list
rearrange.list=function(data,match,...){
  data.res=rearrange.numeric(data=data,match=do.call(c,match))
  return(data.res)
}


#' @S3method rearrange data.frame
#' @method rearrange data.frame
rearrange.data.frame=function(data,match,...){
  data.res=rearrange.list(data=data,match=as.list(match))
  return(data.res)
}

#' @title Interactively rearrange order in data when names are specified.
#' @param data data to rearrange.
#' @param ... names of the variable to rearrange
#' @param order boolean indicating if function \code{\link{arrange}} should be run as well.
#' @export
rearrange2 <- function(data,...,order=T){
  if(order) data <- arrange(data,...)
  cols <- sapply(as.list(substitute(list(...))[-1]),deparse)
  names <- names(data)
  if (length(unmatched<-setdiff(cols,names))>0) warning(unmatched," do not appear in the data frame.")
  cbind(data[intersect(cols,names)],data[!names%in%cols])
}


#' @title Add row.names to a data frame as a new column
#' @param data the data frame
#' @param position position of the new column (1 by default)
#' @param va.name name of the newly created column. By default \code{var.name=".row"}.
#' @seealso \code{\link{col.as.row.names}}
#' @export
row.names.as.col <- function(data,position=1,var.name){
  data$.row=row.names(data)
  data=rearrange(data,match=c(.row=position))
  if(missing(var.name)){ 
    message("The name of the new column is '.row'. Specify 'var.name' to change it.")
  }
  else{
    data=rename(data,c(".row"=var.name))
  }
  row.names(data)=seq(nrow(data))
  data
}

#' @title Set data frame row names
#' @param data the data frame
#' @param names new row names
#' @export
set.row.names <- function(data,names=NULL){
  row.names(data)<-names
  data
}

#' @title Put column values in row names
#' @description
#' This function is the inverse of \code{\link{row.names.as.col}}
#' @param data the data frame
#' @param select the column position (can be an integer or a character string corresponding to the column name)
#' @seealso \code{\link{row.names.as.col}}
#' @export
col.as.row.names <- function(data,select){
  if(!is.numeric(select)){
    #nl=as.list(seq_along(data));
    #names(nl)=names(data);
    #select=eval(substitute(select),nl,parent.frame());
    select=which(select==names(data))
  }
  if(length(select)>1) stop("select must be of size 1.")
  
  row.names(data)=data[[select]]
  data=data[-select]
  data
}