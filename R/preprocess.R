#' @export
preprocess <- function(DATA){
  var.lvs <<- var.vlvs(DATA)
  DATA <- var.autofac(DATA)
  var.fac <<- var.vfac(DATA)
  var.num <<- var.vnum(DATA)
  return(DATA)
}
