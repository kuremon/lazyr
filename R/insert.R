#' @title Insert values within a vector at specified positions
#' @description 
#' \code{insert} inserts values within a vector at specified position.
#' @param v a vector.
#' @param values vector to add to \code{v}.
#' @param index position index vector the same length of \code{values}.
#' @return The output vector \code{v.res} is such that \code{v.res[index]=values} and \code{v.res[-index]=v}.
#' @export
insert=function(v,values,index){
  if(length(values)!=length(index)) stop("values and index must have the same length.");
  if(length(values)==0) return(v)
  if(anyDuplicated(index)!=0) stop("elements of index are not unique.");
  order_of_index=order(index);
  index=index[order_of_index];
  values=values[order_of_index];
  
  length.v=length(v);
  length.index=length(index);
  if((index[1]<1)|(length.v+length.index<tail(index,1))) warning("index takes values outside the specified range.", call.=F)
  new.order=order(c(seq(v),index-seq(0.5:(length.index-0.5))));
  
  v.res=c(v,values);                
  v.res=v.res[new.order];
  return(v.res);
}


#' @title Insert rows in a data frame
#' @description 
#' Insert one or more rows in a data frame 
#' @param data the data.frame to update
#' @param new.rows the rows (data frame) to add (must match the format of \code{data}).
#' @param index the indexes at which the new rows should be added. By default the new rows are added at the end of the \code{data}.
#' @details
#' For values of \code{index} strictly negative, the corresponding \code{new.rows} are added at the \code{abs(where)} positions
#' before the end of \code{data}. For example, for a single row, if \code{index=-1}, the new row is added at the penultimate 
#' position of the existing data frame.
#' @return The updated data frame.
#' @seealso \code{\link{insert}}
#' @export
insert.row=function(data,new.rows,index=NULL){
  data=rbind(data,new.rows);
  if(!is.null(index)){
    n=nrow(data);
    r=r+ifelse(r<0,n,0);
    index.new=insert(v=1:n,values=(n+1):(n+length(index)),index=index);
    data=data[index.new,];
    row.names(data)=1:n;
  }
  return(data);
}



