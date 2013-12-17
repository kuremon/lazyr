#' @title Format number as percentage
#' @export
as.percent <- function(x, digits = 0, format = "f", ...){
  paste(formatC(100 * x, format = format, digits = digits, ...), "%", sep = "")
}