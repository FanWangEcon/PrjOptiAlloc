#' Test Dataset Job Training Effects Lalonde AER 1986
#'
#' trt, a numeric vector identifying the study in which the subjects were enrolled (0 = Control, 1 = treated).
#' age, age (in years).
#' educ, years of education.
#' black, (0 = not black, 1 = black).
#' hisp, (0 = not hispanic, 1 = hispanic).
#' marr, (0 = not married, 1 = married).
#' nodeg, (0 = completed high school, 1 = dropout).
#' re74, real earnings in 1974.
#' re75, real earnings in 1975.
#' re78, real earnings in 1978.
#'
#' @docType data
#'
#' @usage data(df_opt_lalonde_training)
#'
#' @format csv
#'
#' @keywords datasets
#'
#' @references Lalonde, R. (1986) "Evaluating the Econometric Evaluations of Training Programs with Experimental Data", American Economic Review, 604–620.
#'
#' @source \url{https://www.jstor.org/stable/1806062?}
#'
#' @examples
#' data(df_opt_lalonde_training)
#' head(df_opt_lalonde_training, 10)
"df_opt_lalonde_training"
