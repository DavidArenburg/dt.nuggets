#' Fast rolling product
#' @param DT	The data.table to operate on
#' @param col A quoted column name
#' @param N An integer number that we want to shift by
#' @param Name (optional) the name of the new column
#' @param by (optional) A quoted by parameter
#' @param partial (boolean) - if you want partial prod too
#' @param na.rm (boolean) - if you want a ignore NAs (this will run slightly slower)
#' @param ... Some additional parameters that you can pass to the \code{\link{shift}} function
#' @return The modified data.table with the new shifted columns
#' @export
#' @examples  
#' set.seed(123)
#' DT <- data.table(x = sample(10), y = sample(1:2, 10, replace = TRUE), key = "y")
#' frollprod(DT, "x", 3, by = "y", type = "lead")

frollprod <- function(DT, col, N, Name , by, partial = FALSE, na.rm = FALSE, ...){
  
  if(missing(Name)) new_col <- paste0(col, "_prod_", N)  else new_col <- Name
  
  if(partial) fill. <- 1L else fill. <- NA
  
  if(missing(by)) {
    
    if(na.rm) {
      
      DT[, (new_col) := matrixStats::rowProds(as.matrix(setDT(shift(eval(as.name(col)), 0L : (N - 1L), ...))), na.rm = TRUE)]
      
      if(!partial) DT[1L:(N - 1L), (new_col) := NA_real_]
      
    } else DT[, (new_col) := Reduce(`*`, shift(eval(as.name(col)), 0L:(N - 1L), fill = fill., ...))]
    
  } else {
    
    if(na.rm) {
      
      DT[, (new_col) := matrixStats::rowProds(as.matrix(setDT(shift(eval(as.name(col)), 0L : (N - 1L), ...))), na.rm = TRUE), by = by]
      
      if(!partial) {
        
        indx <- seq_len(N - 1L)
        DT[, (new_col) := replace(eval(as.name(new_col)), indx, NA_real_), by = by]
        
      }
      
    } else DT[, (new_col) := Reduce(`*`, shift(eval(as.name(col)), 0L:(N - 1L), fill = fill., ...)), by = by]
    
  }
  
}
