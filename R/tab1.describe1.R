#' @export
tab1.describe1 <- function (DESIGN, STRATA=NULL, CONDIGITS = 1, CATDIGITS = 1, FUNC = "mean", TYPE = 1, MODE = 1) {
  vals <- c()
  val <- c()
  vars <- names(DESIGN[["variables"]])
  if(!is.null(STRATA)){
    www <- which(vars == STRATA)
    vars <- vars[-www]
  }
  for (var in vars) {
    # consider weights of complex design
    if (MODE == 1){
      if (any(var.num==var)) {
        if (FUNC == "mean") {
          val <-  svy.msd(var = var, design = DESIGN, digits = CONDIGITS, type = TYPE)
        }
        if (FUNC == "median") {
          val <- svy.miqr(var = var, design = DESIGN, digits = CONDIGITS, type = TYPE)
        }
      }
      if (any(var.fac==var)) {
        val <- svy.nperc1(var = var, design = DESIGN, digits = CATDIGITS)
      }
      vals <- c(vals, val)
    }
    # do not consider weights for n size
    if (MODE == 2){
      if (any(var.num==var)) {
        if (FUNC == "mean") {
          val <- svy.msd(var = var, design = DESIGN, digits = CONDIGITS, type = TYPE)
        }
        if (FUNC == "median") {
          val <- svy.miqr(var = var, design = DESIGN, digits = CONDIGITS, type = TYPE)
        }
      }
      if (any(var.fac==var)) {
        val0 <- DESIGN[["variables"]][[var]] %>% unwtd.nperc1(digits = CATDIGITS)
        loc0 <- str_locate(val0, "\\(")[,1]
        val0 <- substr(val0,1,loc0-1)
        val1 <- svy.nperc1(var = var, design = DESIGN, digits = CATDIGITS)
        loc1 <- str_locate(val1, "\\(")[,1]
        loc2 <- str_locate(val1, "\\)")[,1]
        val1 <- substr(val1,loc1,loc2)
        val <- paste0(val0, val1)
      }
      vals <- c(vals, val)
    }
    # do not consider complex design
    if (MODE == 3){
      if (any(var.num==var)) {
        if (FUNC == "mean") {
          val <- DESIGN[["variables"]][[var]] %>% unwtd.msd(digits = CONDIGITS, type = TYPE)
        }
        if (FUNC == "median") {
          val <- DESIGN[["variables"]][[var]] %>% unwtd.miqr(digits = CONDIGITS, type = TYPE)
        }
      }
      if (any(var.fac==var)) {
        val <- DESIGN[["variables"]][[var]] %>% unwtd.nperc1(digits = CATDIGITS)
      }
      vals <- c(vals, val)
    }
  }
  mat <- matrix(vals, ncol = 1)
  if(MODE == 1){
    colnames(mat) <- paste("Overall", paste0("(n=", formatC(svy.totaln(DESIGN), format = "f", big.mark = ",", digits = CATDIGITS), ")"), sep = " ")
  }
  if(MODE == 2 | MODE == 3){
    colnames(mat) <- paste("Overall", paste0("(n=", formatC(nrow(DESIGN[["variables"]]), format = "f", big.mark = ",", digits = 0), ")"), sep = " ")
  }
  return(mat)
}
