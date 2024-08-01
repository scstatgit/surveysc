#' @export
svy.ttest <- function (y, x, design) {
  mf <- as.formula(paste0(y, "~", x))
  res <- svyttest(mf, dclus1)
  pval <- sprintf("%.3f", res[["p.value"]])
  pval <- data.frame(pval = pval, method = "svy.ttest")
  return(pval)
}
