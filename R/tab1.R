#' table1 function
#'
#' This function create table1 in biostatistics
#' @param DESIGN: put survey data which is svydesign() from survey package
#' @param STRATA: put stratification varaible
#' @param OVERALL: if you want overall statistics, set TRUE
#' @param CONDIGITS: digits of continuous variable
#' @param CATDIGITS: digits of categorical variable
#' @param FUNC: "mean" or "median"
#' @param TYPE: type of print FUNC
#' @param MARGIN: margin of percentage. 1 as row percent, and 2 as col percent
#' @param MODE: mode of descriptive statistics. 1 as consider weights of complex design, 2 as do not consider weights for n size, 3 as do not consider complex design.
#' @keywords create table1 in complex survey design
#' @examples
#' options(survey.lonely.psu = "adjust")
#' options(survey.adjust.domain.lonely = TRUE)
#' ddd <- select(apiclus2, dnum,pw,fpc1,api00,api99,target,sch.wide,comp.imp,meals)
#' ddd <- preprocess(ddd)
#' ddd$fpc1 <- as.numeric(as.character(ddd$fpc1))
#' dclus1 <- svydesign(id=~dnum, weights=~pw, data=ddd, fpc=~fpc1)
#' tab1(dclus1, MODE = 1)
#' tab1(dclus1, MODE = 2)
#' tab1(dclus1, MODE = 3)
#' tab1(dclus1, STRATA = "sch.wide")
#' tab1(dclus1, STRATA = "sch.wide", MODE = 1)
#' tab1(dclus1, STRATA = "sch.wide", MODE = 2)
#' tab1(dclus1, STRATA = "sch.wide", MODE = 3)
#' tab1(dclus1, STRATA = "sch.wide", MARGIN = 1, OVERALL = T)
#' @export
tab1 <- function (DESIGN, STRATA = NULL, OVERALL = FALSE, CONDIGITS = 1, CATDIGITS = 1, FUNC = "mean", TYPE = 1, MARGIN = 2, MODE = 1) {
  mat1 <- tab1.describe0(DESIGN, var.lvs, STRATA)
  mat2 <- tab1.describe1(DESIGN, STRATA, CONDIGITS = CONDIGITS, CATDIGITS = CATDIGITS, FUNC = FUNC, TYPE = TYPE, MODE = MODE)
  if (is.null(STRATA)) {
    res <- cbind(mat1, mat2) %>% as.data.frame()
  }
  if (!is.null(STRATA)) {
    mat3 <- tab1.describe2(DESIGN, STRATA = STRATA, CONDIGITS = CONDIGITS, CATDIGITS = CATDIGITS, FUNC = FUNC, TYPE = TYPE, MARGIN = MARGIN, MODE = MODE)
    res <- cbind(mat1, mat3) %>% as.data.frame()
    # add pvals
    t1.pval <- tab1.pval(design = DESIGN, strata = STRATA)
    res <- left_join(res, t1.pval, by = "Characteristics") %>%
      group_by(Characteristics) %>%
      mutate(
        pval = case_when(
          row_number()==1 ~ pval,
          TRUE ~ ""
        ),
        method = case_when(
          row_number()==1 ~ method,
          TRUE ~ ""
        )
      ) %>%
      as.data.frame()
  }
  if (!is.null(STRATA) & isTRUE(OVERALL)) {
    mat3 <- tab1.describe2(DESIGN, STRATA = STRATA, CONDIGITS = CONDIGITS, CATDIGITS = CATDIGITS, FUNC = FUNC, TYPE = TYPE, MARGIN = MARGIN, MODE = MODE)
    res <- cbind(mat1, mat2, mat3) %>% as.data.frame()
    # add pvals
    t1.pval <- tab1.pval(design = DESIGN, strata = STRATA)
    res <- left_join(res, t1.pval, by = "Characteristics") %>%
      group_by(Characteristics) %>%
      mutate(
        pval = case_when(
          row_number()==1 ~ pval,
          TRUE ~ ""
        ),
        method = case_when(
          row_number()==1 ~ method,
          TRUE ~ ""
        )
      ) %>%
      as.data.frame()
  }
  return(res)
}

