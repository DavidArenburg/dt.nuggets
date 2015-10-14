#' Fast removing unessacery levels from factor columns
#' @param DT	The data.table to operate on
#' @param cols (optional) Quoted column names
#' @return The modified data.table with the refactored columns
#' @export
#' @examples  
#' set.seed(123)
#' DT <- data.table(x = sample(10), 
#'                  y = sample(1:2, 10, replace = TRUE),
#'                  z = factor(sample(letters, 10)),
#'                  l = factor(sample(LETTERS, 10)))
#' 
#' SDT <- DT[sample(5)]
#' str(SDT)
#' refactor(SDT)
#' str(SDT)
#' SDT <- DT[sample(5)]
#' str(SDT)
#' refactor(SDT, "z")
#' str(SDT)


refactor <- function(DT, cols){
  if(missing(cols)) {
    for(j in names(DT)[sapply(DT, is.factor)]){
      set(DT, j = j, value = factor(DT[[j]]))
    }
  } else {
      for(j in cols) {
        set(DT, j = j, value = factor(DT[[j]]))
      }
    }
}