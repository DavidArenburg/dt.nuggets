#' Fast converting from factors to numeric/integers
#' @param DT	The data.table to operate on
#' @param cols Quoted column names
#' @param class Which class to convert to: integer or numeric?
#' @param removecomma Should remove commas?
#' @return The modified data.table with the refactored columns
#' @export
#' @examples 
#' set.seed(123)
#' DT <- data.table(x = factor(sample(10)), 
#'                  y = factor(sample(10) + round(runif(10), 3)),
#'                  z = factor(paste(sample(10), round(runif(10, 100, 1000), 3), sep = ",")),
#'                  l = factor(paste(sample(10), round(runif(10, 100, 1000), 3), sep = ",")))
#' DT
#' str(DT)
#' factonum(DT, "x", "integer")
#' factonum(DT, "y")
#' factonum(DT, c("z", "l"), removecomma = TRUE)
#' str(DT)
#' DT


factonum <- function(DT, cols, class = "numeric", removecomma = FALSE){
  stopifnot(!missing(cols), 
            all(cols %in% names(DT)), 
            class %in% c("numeric", "integer"),
            is.logical(removecomma))
  if(isTRUE(removecomma)) {
    if(class == "numeric") {
      for(j in cols) {
        set(DT, j = j, value = as.numeric(gsub(",", "", levels(DT[[j]]), fixed = TRUE))[DT[[j]]])
      } 
    } else {
        for(j in cols) {
          set(DT, j = j, value = as.integer(gsub(",", "", levels(DT[[j]]), fixed = TRUE))[DT[[j]]])
        } 
    }
  } else {
    if(class == "numeric") {
      for(j in cols) {
        set(DT, j = j, value = as.numeric(levels(DT[[j]]))[DT[[j]]])
      } 
    } else {
      for(j in cols) {
        set(DT, j = j, value = as.integer(levels(DT[[j]]))[DT[[j]]])
      } 
    }
  }
}

