#' Test Dataset California Test Score Data: \strong{PREP I Frame}, Select Input and Estimation Dates
#'
#' \eqn{\text{ID}_i}: individual id
#' \eqn{D^{\text{max}}_{i}}: maximimum discrete allocation each person, in addition to existing levels
#' \eqn{D^{\text{o}}_{i}}: what I call observed allocation, just any comparison allocation,
#' in addition to existing levels, this is the additional allocation (ignoring existing levels),
#' this is the not the total allocation. If fully redistributing, this could be the observed level of allocation.
#' \eqn{\Omega_{i}}: This is the expected outcome when the input of interest is completely zero,
#' \eqn{\Omega_{i}\neq A_{i,l=0}}, because \eqn{l=0} does not mean input is zero, using \eqn{\Omega_i} rather
#' than \eqn{A_i} to avoid confusion because \eqn{A_i} sounds like \eqn{A_i = A_{i, l=0}}, but again, the problem
#' does not start with allocation at zero, but each district already has some existing number of teachers.
#' \eqn{\theta_{i}}: coefficient in front of student teacher ratio
#' \eqn{\beta_{i}}: i specific preference
#' \eqn{\text{enrltot}_{i}}: enrollment per district/school, assume no within district variations
#' \eqn{\text{teachers}_{i}}: teacher per distrct/school
#' \eqn{\text{stravg}_{i}}: average of student teacher ratio district/school
#'
#' @docType data
#'
#' @usage data(df_opt_caschool_prep_i)
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
#' data(df_opt_caschool_prep_i)
#' head(df_opt_caschool_prep_i, 10)
"df_opt_caschool_prep_i"
