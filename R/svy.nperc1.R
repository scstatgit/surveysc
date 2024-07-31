#' @export
svy.nperc1 <- function (var, design, digits = 0){
  mf <- as.formula(paste0("~",var))
  xt <- svytable(mf, design)
  pt0 <- prop.table(xt)
  res <- sprintf(paste0("%.", digits, "f", " (%.", digits, "f%%)"), xt, pt0 * 100)
  return(res)
}
