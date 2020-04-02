#' Test Dataset California Test Score Data: \strong{IB Frame}, Alternative Allocation
#'
#' \eqn{\text{ID}_i}: individual id
#' \eqn{A_{i,l=0}}: A when considering the lth allocation for ith person, expected outcome without the lth allocation
#' \eqn{\alpha^{\text{o}}_{i}}: see \emph{df_casch_prep_i}, observed/random/uniform allocation's marginal effect.
#' Note since \eqn{\alpha_{il}} considers only additional allocation, with existing allocations embeded in \eqn{D_{il}},
#' \eqn{\alpha^{\text{o}}_{i}} also only includes additional allocations that is not already embeded in \eqn{\A_{i,l=0}}.
#' \eqn{\beta_{i}}: i specific preference
#'
#' @docType data
#'
#' @usage data(df_opt_caschool_input_ib)
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
#' data(df_opt_caschool_input_ib)
#' head(df_opt_caschool_input_ib, 10)
"df_opt_caschool_input_ib"
