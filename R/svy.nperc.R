#' @export
svy.nperc <- function (DESIGN, var1, var2, digits = 0, type = 0) {
  mf <- as.formula(paste0("~",var1,"+",var2))
  xt <- svytable(mf, DESIGN)
  pt0 <- prop.table(xt, margin = NULL)
  pt1 <- prop.table(xt, margin = 1)
  pt2 <- prop.table(xt, margin = 2)
  if (type == 0) {
    res <- sprintf(paste0("%.", digits, "f", " (%.", digits, "f%%)"), xt, pt0 * 100)
  }
  if (type == 1) {
    res <- sprintf(paste0("%.", digits, "f", " (%.", digits, "f%%)"), t(xt), t(pt1) * 100)
  }
  if (type == 2) {
    res <- sprintf(paste0("%.", digits, "f", " (%.", digits, "f%%)"), t(xt), t(pt2) * 100)
  }
  return(res)
}
