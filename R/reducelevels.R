#' Reducing the amount of levels in factor columns
#' @param DT	The data.table to operate on
#' @param cols Quoted column names
#' @param freq The quantile from lower eqaul to you want to replace levels
#' @param newlvl The name of the new level
#' @return The modified data.table with the refactored columns
#' @export
#' @examples 
#' set.seed(123)
#' DT <- data.table(A = factor(sample(letters, 1e3L, replace = TRUE, prob = (1:length(letters))/100)),
#'                  B = factor(sample(letters, 1e3L, replace = TRUE, prob = rev(1:length(letters))/100)),
#'                  C = factor(sample(letters, 1e3L, replace = TRUE, prob = sample(1:length(letters))/100)),
#'                  D = sample(letters, 1e3, replace = TRUE),
#'                  E = sample(1e3))
#' 
#' str(DT)
#' reducelevels(DT)
#' str(DT)
#' reducelevels(DT, c("A", "B"), 0.6)
#' str(DT)


reducelevels <- function(DT, cols, freq = 0.1, newlvl = "OTHER"){
  TDT <- copy(DT)
  if(missing(cols)) {
    for(j in names(DT)[sapply(DT, is.factor)]){
      temp <- tabulate(TDT[[j]])
      levels(TDT[[j]])[temp <= quantile(temp, freq)] <- newlvl
      set(DT, j = j, value = TDT[[j]])
    }
  } else {
    stopifnot(all(sapply(DT[, cols, with = FALSE], is.factor)))
    for(j in cols){
      temp <- tabulate(TDT[[j]])
      levels(TDT[[j]])[temp <= quantile(temp, freq)] <- newlvl
      set(DT, j = j, value = TDT[[j]])
    }
  }
}
