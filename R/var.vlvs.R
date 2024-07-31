#' @export
var.vlvs <- function(DATA){
  res <- sapply(DATA, function(var) var.nlevels(DATA,var))
  return(res)
}
