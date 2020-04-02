#' Test Dataset California Test Score Data: \strong{IL Frame}, Generated Effect Each Additional Allocation
#'
#' \eqn{\text{ID}_i}: individual id
#' \eqn{\text{ID}_{il}}: unique id for individual/allocation id
#' \eqn{D^{\text{max}}_{i}}: maximimum discrete allocation each person
#' \eqn{D_{il}}: lth discrete allocation level for ith person, if fully redistributing, then this is equal
#' to toal allocation count, if this is distribution additional teachers, this is equal to the additional allocation of teachers for each school.
#' \eqn{A_{il}}: A when considering the lth allocation for ith person, expected outcome without the lth allocation
#' \eqn{\alpha_{il}}: marginal expected effect of the lth allocation
#' \eqn{\beta_{i}}: i specific preference
#'
#' @docType data
#'
#' @usage data(df_opt_caschool_input_il)
#'
#' @format csv
#'
#' @keywords datasets
#'
#' @references Stock, James H. and Mark W. Watson (2003) Introduction to Econometrics, Addison-Wesley Educational Publishers, chapter 4–7.
#'
#' @source
#' \url{https://fmwww.bc.edu/ec-p/data/stockwatson/caschool.des}
#' \url{https://fanwangecon.github.io/PrjOptiAlloc/articles/ffv_opt_sodis_rkone_casch_allrw.html}
#'
#' @examples
#' data(df_opt_caschool_input_il)
#' head(df_opt_caschool_input_il, 10)
"df_opt_caschool_input_il"
