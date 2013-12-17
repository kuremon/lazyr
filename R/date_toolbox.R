d.base=function(day,month,year){
  if(nchar(day)<2)
    {day=paste0("0",day)};
  if(nchar(month)<2)
    {month=paste0("0",month)};
  return(as.Date(paste(day,month,year,sep="-"),format="%d-%m-%Y"));
}

#' @title Create a Date object
#' @description 
#' Create date objects with an easy syntax 
#' @param days vector of days. Can be code{numeric} or \code{character}
#' @param months vector of months.
#' @param years the vector of years.
#' @param expand a boolean (see Details section).
#' @details
#' If \code{expand} is \code{TRUE}, then all the combanition of dates from the input vectors are creator.
#' If \code{expand} is \code{FALSE}, then the smaller vectors are replicated to match the length of the longest vector (\code{N})
#' and \code{N} dates are created such that the i-th date is \code{d(days[i],months[i],years[i])}. 
#' @return A vector of object of class \code{Date}.
#' @seealso \code{\link{is.integer}}
#' @export
d=function(days,months,years,expand=F){
  if(expand){
    expanded.param=as.list(as.data.frame(t(expand.grid(days,months,years))));
    res.list=lapply(expanded.param,function(r)d.base(r[1],r[2],r[3]));
  }else{
    d.vectorized=Vectorize(d.base,SIMPLIFY=F);
    res.list=d.vectorized(days,months,years);
  }
  res=do.call(c,res.list);
  names(res)=NULL;
  return(res);
}

