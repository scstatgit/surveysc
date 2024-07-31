#' @export
svy.miqr <- function(var,design,digits=3,type=1,deff=NULL){
  mf <- as.formula(paste("~",var,collapse="+"))
  min <- svyquantile(mf, dclus1, quantiles = seq(0,1,.25), na.rm=T, ci=F)[[1]][1]
  Q1 <- svyquantile(mf, dclus1, quantiles = seq(0,1,.25), na.rm=T, ci=F)[[1]][2]
  Q2 <- svyquantile(mf, dclus1, quantiles = seq(0,1,.25), na.rm=T, ci=F)[[1]][3]
  Q3 <- svyquantile(mf, dclus1, quantiles = seq(0,1,.25), na.rm=T, ci=F)[[1]][4]
  max <- svyquantile(mf, dclus1, quantiles = seq(0,1,.25), na.rm=T, ci=F)[[1]][5]
  if(type==1){
    res <- sprintf("%.3f [%.3f, %.3f]", Q2, Q1, Q3)
  }
  if(type==2){
    res <- sprintf("%.3f (%.3f - %.3f)", Q2, min, max)
  }
  return(res)
}
