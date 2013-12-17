within_interval=function(x, interval, option){
  if(diff(interval)<0){warning("The interval[1]>interval[2]. NA returned.",call.=F); return(NA);}
  res=switch(option,
             "cc"=(interval[1]<=x)&(x<=interval[2]),
             "oc"=(interval[1]<x)&(x<=interval[2]),
             "co"=(interval[1]<=x)&(x<interval[2]),
             "oo"=(interval[1]<x)&(x<interval[2]))
  return(res);
}

#' @title Test for interval membership
#' @rdname within
#' @description 
#' This function tests if a \code{x} belongs to the interval \code{interval}.
#' @param x the numeric value to test.
#' @param interval the interval to test.
#' @details 
#' \code{\%within\%} tests for a closed interval
#' @return A boolean
#' @seealso \code{\link{\%in\%}}
#' @export
"%within%"<-function(x, interval){
  within_interval(x,interval,"cc");
}

#' @rdname within
#' @details
#' \code{\%o_within\%} tests for an left-open right-close interval 
#' @export
"%o_within%"<-function(x, interval){
  within_interval(x,interval,"oc");
}

#' @rdname within
#' @details
#' \code{\%within_o\%} tests for a left-close right-open interval 
#' @export
"%within_o%"<-function(x, interval){
  within_interval(x,interval,"co");
}

#' @rdname within
#' @details
#' \code{\%o_within_o\%} or \code{\%owo\%} tests for an open interval
#' @export
"%o_within_o%"<-function(x, interval){
  within_interval(x,interval,"oo");
}

#' @rdname within
#' @export
"%owo%"<-function(x,interval){
  within_interval(x,interval,"oo");
}
