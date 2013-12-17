#' @title Glue two data frames
#' @description Merge two data frames by common columns such that the original layout of the first data frame is retained and
#' the extra information contained in the second is added to the right.
#' @param x, y  data frames, or objects to be coerced to one.
#' @param by, by.x, by.y specifications of the columns used for merging
#' @param all.x  logical; if TRUE, then extra rows will be added to the output, one for each row in \code{x} that has no 
#' matching row in \code{y}. These rows will have \code{NA}s in those columns that are usually filled with values from 
#' \code{y}. The default is \code{TRUE}, so that all rows of \code{x} are included in the output.
#' @param suffixes  a character vector of length 2 specifying the suffixes to be used for making unique the names of columns
#' in the result which not used for merging (appearing in \code{by}).
#' @export
#' @examples
#' \donttest{
#' info.gear=data.frame(am=0:1,name=c("manual","automatic"),info=c("Worldwide","Mainly US"))
#' glue(mtcars,info.gear)
#' glue(mtcars,cbind(info.gear,wt="not specified"),by="am")
#' info.iris=data.frame(Species=c("versicolor","virginica","sibirica"),Origin=c("North America","Florida, Georgia","Russia"))
#' glue(iris,info.iris)
#' glue(iris,info.iris,all.x=FALSE)
#' }
glue=function(x,y,by=intersect(names(x),names(y)),by.x=by,by.y=by,all.x=TRUE,suffixes=c(".x",".y")){
  if(anyDuplicated(y[by.y])!=0){
    warning("Duplicates present in 'y[by.y]'.",call.=FALSE)
    y=y[!duplicated(y[by.y]),]
  }
  match.idx=match(x[[by.x]],y[[by.y]])
  
  y=y[names(y)!=by.y]
  has.common.names=any(cnm.x<-names(x)%in%names(y))
  if(has.common.names){
    warning("Common names in data 'x' and 'y'.",call.=FALSE)
    cnm.y=names(y)%in%names(x);
    names(x)[cnm.x]=paste0(names(x)[cnm.x],suffixes[1L]);
    names(y)[cnm.y]=paste0(names(y)[cnm.y],suffixes[2L]);
  }
  
  if(!all.x){
    valid.match.idx=!is.na(match.idx)
    x=x[valid.match.idx,]
    match.idx=match.idx[valid.match.idx]
  }
  
  cbind(x,y[match.idx,,drop=FALSE])
}