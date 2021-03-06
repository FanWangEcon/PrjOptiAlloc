% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ffd_opt_lalonde_training_wage.R
\docType{data}
\name{df_opt_lalonde_training_wage}
\alias{df_opt_lalonde_training_wage}
\title{Lalonde AER 1986 with Wage Regression Allocation Queue}
\format{
csv
}
\source{
\url{https://fanwangecon.github.io/PrjOptiAlloc/articles/ffv_opt_sobin_rkone_allrw_training_wage.html}
}
\usage{
data(df_opt_lalonde_training_wage)
}
\description{
appended onto the raw Lalonde binary targeting queue rank info based on different planner inequality aversion.
includes min and max rank. Based on wage regression.
}
\examples{
data(df_opt_lalonde_training_wage)
head(df_opt_lalonde_training_wage, 10)
}
\references{
Lalonde, R. (1986) "Evaluating the Econometric Evaluations of Training Programs with Experimental Data", American Economic Review, 604–620.
}
\keyword{datasets}
