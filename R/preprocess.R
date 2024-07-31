#' @export
preprocess <- function(DATA){
  var.lvs <<- var.vlvs(DATA)
  DATA <- var.autofac(DATA)
  var.fac <<- var.fac(DATA)
  var.num <<- var.num(DATA)
  return(DATA)
}
