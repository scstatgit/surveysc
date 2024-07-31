#' @export
lst <- c()
tab1.describe0 <- function (DESIGN, LVS, STRATA=NULL) {
  row.lvs <- LVS
  row.lvs[which(!(names(row.lvs) %in% var.fac))] <- 1
  vars <- names(DESIGN[["variables"]])
  if(!is.null(STRATA)){
    www <- which(vars == STRATA)
    vars <- vars[-www]
    row.lvs <- row.lvs[-www]
  }
  for (var in vars) {
    if (row.lvs[var] == 1) {
      res <- ""
    }
    if (row.lvs[var] >= 2) {
      res <- var.levels(DESIGN[["variables"]], var) %>% as.character()
    }
    idx <- which(names(row.lvs) == var)
    lst[[idx]] <- res
  }
  unlist(lst)
  nm <- rep(names(row.lvs), row.lvs)
  cumsum(row.lvs)
  nrow <- cumsum(row.lvs)[length(cumsum(row.lvs))]
  mat <- matrix("", ncol = 2, nrow = nrow)
  mat[, 1] <- nm
  mat[, 2] <- unlist(lst)
  colnames(mat) <- c("Characteristics", "Levels")
  return(mat)
}
