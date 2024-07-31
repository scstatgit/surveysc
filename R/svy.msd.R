#' @export
svy.msd <- function(var,design,digits=3,type=NULL,deff=NULL){
  mf <- as.formula(paste("~",var,collapse="+"))
  mean <- svymean(mf, design, na.rm = T)
  sd <- svyvar(mf, design, na.rm = T)[[1]] %>% sqrt()
  res <- sprintf(paste0("%.",digits,"f", " \U00B1 ", "%.",digits,"f"), mean, sd)
  return(res)
}
