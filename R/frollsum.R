#' Fast rolling sum
#' @param DT	The data.table to operate on
#' @param col A quoted column name
#' @param N An integer number that we want to shift by
#' @param Name (optional) the name of the new column
#' @param partial boolean - to do a partial sum or not 
#' @param by (optional) A quoted by parameter
#' @param ... Some additional parameters that you can pass to the \code{\link{shift}} function
#' @return The modified data.table with the new shifted columns
#' @export
#' @examples  
#' set.seed(123)
#' DT <- data.table(x = sample(10), y = sample(1:2, 10, replace = TRUE), key = "y")
#' frollsum(DT, "x", 3, by = "y", type = "lead")

frollsum <- function(DT, col, N, Name = "Sum", by, partial = FALSE, ...){
  
  if(partial) fill. <- 0L else fill. <- NA
  
  if(missing(by)) {
    
    return(DT[, paste0(Name, N) := Reduce(`+`, shift(eval(as.name(col)), 0L:(N - 1L), fill = fill., ...))])
    
  }
  
  DT[, paste0(Name, N) := Reduce(`+`, shift(eval(as.name(col)), 0L:(N - 1L), fill = fill., ...)), by = by]
  
}


