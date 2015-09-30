#' Fast rolling product
#' @param DT	The data.table to operate on
#' @param col A quoted column name
#' @param N An integer number that we want to shift by
#' @param Name (optional) the name of the new column
#' @param type A string specifying the shift direction: "lag"/"lead"
#' @param by (optional) A quoted by parameter
#' @return The modified data.table with the new shifted columns
#' @export
#' @examples  
#' set.seed(123)
#' DT <- data.table(x = sample(10), y = sample(1:2, 10, replace = TRUE), key = "y")
#' frollprod(DT, "x", 3, by = "y")


frollprod <- function(DT, col, N, Name = "Prod", type = "lead", by){
  if(missing(by)) {
    return(DT[, paste0(Name, N) := Reduce(`*`, shift(eval(as.name(col)), 0L:(N - 1L), type = type))])
  }
  DT[, paste0(Name, N) := Reduce(`*`, shift(eval(as.name(col)), 0L:(N - 1L), type = type)), by = by]
}


