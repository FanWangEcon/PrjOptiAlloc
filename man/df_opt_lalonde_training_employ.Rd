% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ffd_opt_lalonde_training_employ.R
\docType{data}
\name{df_opt_lalonde_training_employ}
\alias{df_opt_lalonde_training_employ}
\title{Lalonde AER 1986 with Wage Regression Allocation Queue}
\format{
csv
}
\source{
\url{https://fanwangecon.github.io/PrjOptiAlloc/articles/ffv_opt_sobin_rkone_allfc_training_logit.html}
}
\usage{
data(df_opt_lalonde_training_employ)
}
\description{
appended onto the raw Lalonde binary targeting queue rank info based on different planner inequality aversion.
includes min and max rank. Based on training logit regressin.
}
\examples{
data(df_opt_lalonde_training_employ)
head(df_opt_lalonde_training_employ, 10)
}
\references{
Lalonde, R. (1986) "Evaluating the Econometric Evaluations of Training Programs with Experimental Data", American Economic Review, 604–620.
}
\keyword{datasets}
