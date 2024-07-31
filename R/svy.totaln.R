#' @export
svy.totaln <- function(DESIGN){
  weight <- DESIGN[["prob"]]^-1
  raw.n <- DESIGN[["strata"]]
  nn <- weight*raw.n
  res <- sum(nn)
  return(res)
}
