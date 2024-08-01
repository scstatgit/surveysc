#' @export
tab1.pval <- function(design, strata){
  vars <- names(design[["variables"]])
  vars <- vars[!(vars == strata)]
  pvals <- c()
  for (var in vars){
    if(any(var.num==var)){
      pval <- svy.ttest(var, strata, design)
      pval <- data.frame(Characteristics = var, pval = sprintf("%.3f", pval[1]), method = pval[2])
      pvals <- rbind(pvals, pval)
    }
    if(any(var.fac==var) & var.lvs[var]>=2){
      pval <- svy.chisq(var, strata, design)
      pval <- data.frame(Characteristics = var, pval = sprintf("%.3f", pval[1]), method = pval[2])
      pvals <- rbind(pvals, pval)
    }
    if(any(var.fac==var) & var.lvs[var]==1){
      pval <- data.frame(Characteristics = var, pval = "", method = "")
      pvals <- rbind(pvals, pval)
    }
  }
  return(pvals)
}
