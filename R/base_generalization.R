#' @title n-ary set union
#' @description
#' Performs set union on an unspecified number of arguments
#' @param ... vectors (of the same mode)
#' @return The union of all vectors.
#' @seealso \code{\link{Reduce_operator}}
#' @export
#' @examples
#' \donttest{
#' n.union(1:5)
#' n.union(1:5,61) # same as base::union
#' n.union(1:5,61,3:6)
#' }
n.union=recurs::Reduce_operator(base::union)

expand.mgrid.base=function(...){
  args=list(...)
  x=as.data.frame(args[[1]],stringsAsFactors = FALSE)
  y=as.data.frame(args[[2]],stringsAsFactors = FALSE)
  
  nx=nrow(x)
  ny=nrow(y)
  
  unrowname(cbind(x[rep(seq(nx),each=ny),,drop=FALSE],y[rep(seq(ny),nx),,drop=FALSE]))
}

#' @title expand grid for data frames
#' @description
#' Performs the equivalent of \code{\link{expand.grid}} on data frames.
#' @param ... data frames
#' @return A data.frame with all possible combinations of the original data frames in
#' \code{\link{...}}.
#' @seealso \code{\link{expand.grid}}
#' @export
#' @examples
#' \donttest{
#' x=data.frame(value=0:1)
#' expand.mgrid(mtcars[1:3,c("wt","cyl")],iris[50:51,c("Petal.Width","Species")],x)
#' }
expand.mgrid=recurs::Reduce_operator(expand.mgrid.base)


#' @title Match a vector to a list of vector
#' @seealso \code{\link{match}}
#' @export
#' @examples
#' \donttest{
#' x=letters[1:4]
#' list.table=list("c","e",c("a","b"),c("b","5"))
#' match.in(x,list.table)
#' }
match.in=function(x,list.table){
  apply(do.call(rbind,lapply(list.table,function(l)x%in%l)),2,
        function(col){
          if(all(!col)) return(NA)
          which(col)
        })
}

#' @title Linearly scale a vector of numeric valies to 0-1
#' @param x a numeric vector
#' @export
#' @examples
#' \donttest{
#' scale01(1:10)
#' }
scale01=function(x){
  as.vector(scale(x,min(x),max(x)-min(x)))
}

#' @title Replicate a matrix
#' @rdname repmat
#' @param A matrix to replicate
#' @param m number of row replication
#' @param n number of colum replication
#' @export
repmat.i=function(A,m){
  A=as.matrix(A)
  matrix(rep(t(A),m),ncol=ncol(A),byrow=TRUE) 
}

#' @rdname repmat
#' @export
repmat.j=function(A,n){
  A=as.matrix(A)
  matrix(rep(A,n),nrow=nrow(A),byrow=FALSE) 
}

#' @rdname repmat
#' @export
repmat=function(A,m,n){
  repmat.j(repmat.i(A,m),n)
}