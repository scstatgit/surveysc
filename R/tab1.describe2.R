#' @export
tab1.describe2 <- function(DESIGN, STRATA, CONDIGITS = 1, CATDIGITS = 1, FUNC = "mean", TYPE = 1, MARGIN = 2, MODE = 1) {
  val <- c()
  vals <- c()
  vars <- names(DESIGN[["variables"]])
  www <- which(vars == STRATA)
  vars <- vars[-www]
  for (var in vars) {
    # consider weights of complex design
    if (MODE == 1){
      if (any(var.num==var)) {
        mf <- as.formula(paste("~",var,collapse="+"))
        mf.strata <- as.formula(paste("~",STRATA,collapse="+"))
        if (FUNC == "mean") {
          val <- svyby(mf, mf.strata, design=DESIGN, svy.msd, keep.var = F)[["statistic"]]
        }
        if (FUNC == "median") {
          val <- svyby(mf, mf.strata, design=DESIGN, svy.miqr, keep.var = F)[["statistic"]]
        }
      }
      if (any(var.fac==var)) {
        vec <- svy.nperc(DESIGN, var, STRATA, type = MARGIN, digits = CATDIGITS)
        val <- matrix(vec, ncol = var.lvs[[STRATA]])
      }
      vals <- c(vals, val)
    }
    if (MODE == 2){
      # do not consider weights for n size
      if (any(var.num==var)) {
        mf <- as.formula(paste("~",var,collapse="+"))
        mf.strata <- as.formula(paste("~",STRATA,collapse="+"))
        if (FUNC == "mean") {
          val <- svyby(mf, mf.strata, design=DESIGN, svy.msd, keep.var = F)[["statistic"]]
        }
        if (FUNC == "median") {
          val <- svyby(mf, mf.strata, design=DESIGN, svy.miqr, keep.var = F)[["statistic"]]
        }
      }
      if (any(var.fac==var)) {
        vec0 <- unwtd.nperc(DESIGN[["variables"]], var, STRATA, type = MARGIN, digits = CATDIGITS)
        loc0 <- str_locate(vec0, "\\(")[,1]
        vec0 <- substr(vec0,1,loc0-1)
        val0 <- matrix(vec0, ncol = var.lvs[[STRATA]])
        vec1 <- svy.nperc(DESIGN, var, STRATA, type = MARGIN, digits = CATDIGITS)
        loc1 <- str_locate(vec1, "\\(")[,1]
        loc2 <- str_locate(vec1, "\\)")[,1]
        vec1 <- substr(vec1,loc1,loc2)
        val1 <- matrix(vec1, ncol = var.lvs[[STRATA]])
        val <- paste0(val0, val1)
      }
      vals <- c(vals, val)
    }
    if (MODE == 3){
      if (any(var.num==var)) {
        if(FUNC=="mean"){ val <- tapply(DESIGN[["variables"]][[var]], INDEX = DESIGN[["variables"]][[STRATA]], function(var) unwtd.msd(var, digits = CONDIGITS, type = TYPE)) }
        if(FUNC=="median"){ val <- tapply(DESIGN[["variables"]][[var]], INDEX = DESIGN[["variables"]][[STRATA]], function(var) unwtd.miqr(var, digits = CONDIGITS, type = TYPE)) }
      }
      if(any(var.fac==var)){
        vec <- unwtd.nperc(DESIGN[["variables"]], var, STRATA, type = MARGIN, digits = CATDIGITS)
        val <- matrix(vec, ncol=var.lvs[[STRATA]])
      }
      vals <- c(vals, val)
    }
  }
  mat <- matrix(vals, ncol = var.lvs[[STRATA]], byrow = T)
  nn.strata <- svytable(as.formula(paste0("~",STRATA)), DESIGN)
  nn <- paste0("(n=", formatC(nn.strata, format = "f", big.mark = ",", digits = CATDIGITS), ")")
  colnames(mat) <- paste(var.levels(DESIGN[["variables"]], STRATA), nn, sep = " ")
  return(mat)
}
