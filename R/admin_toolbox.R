#' @title Check if a directory exists
#' @export
dir.exists=function(path){
if(is.null(path)) return(FALSE)
length(list.files(path,all.files=TRUE))>0
}