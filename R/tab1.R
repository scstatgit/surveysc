#' @export
tab1 <- function (DESIGN, STRATA = NULL, OVERALL = FALSE, CONDIGITS = 1, CATDIGITS = 1, FUNC = "mean", TYPE = 1, MARGIN = 2, MODE = 1) {
  mat1 <- tab1.describe0(DESIGN, var.lvs, STRATA)
  mat2 <- tab1.describe1(DESIGN, STRATA, CONDIGITS = CONDIGITS, CATDIGITS = CATDIGITS, FUNC = FUNC, TYPE = TYPE, MODE = MODE)
  if (is.null(STRATA)) {
    res <- cbind(mat1, mat2)
  }
  if (!is.null(STRATA)) {
    mat3 <- tab1.describe2(DESIGN, STRATA = STRATA, CONDIGITS = CONDIGITS, CATDIGITS = CATDIGITS, FUNC = FUNC, TYPE = TYPE, MARGIN = MARGIN, MODE = MODE)
    res <- cbind(mat1, mat3)
  }
  if (!is.null(STRATA) & isTRUE(OVERALL)) {
    mat3 <- tab1.describe2(DESIGN, STRATA = STRATA, CONDIGITS = CONDIGITS, CATDIGITS = CATDIGITS, FUNC = FUNC, TYPE = TYPE, MARGIN = MARGIN, MODE = MODE)
    res <- cbind(mat1, mat2, mat3)
  }
  return(res)
}
