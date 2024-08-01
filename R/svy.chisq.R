#' @export
svy.chisq <- function(y, x, design, method="Chisq"){
  mf <- as.formula(paste0("~",y,"+",x))
  tbl <- svytable(mf, design)
  res <- svychisq(mf, design, statistic = method)
  pval <- data.frame(pval = res$p.value, method = res$method)
  return(pval)
}
