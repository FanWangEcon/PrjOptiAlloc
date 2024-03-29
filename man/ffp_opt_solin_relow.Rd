% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ffp_opt_solin.R
\name{ffp_opt_solin_relow}
\alias{ffp_opt_solin_relow}
\title{Theorem 1, Binary Optimal Allocation solution, for one planner inequality
preference (one rho value)}
\usage{
ffp_opt_solin_relow(
  df,
  svr_A_i,
  svr_alpha_i,
  svr_beta_i,
  fl_N_agg,
  fl_rho,
  svr_inpalc = "optiallocate",
  svr_expout = "optiexpoutcm"
)
}
\arguments{
\item{df}{tibble data table including variables using svr names below each
row is potentially an individual who will receive alternative allocations}

\item{svr_A_i}{string name of the A_i variable, dot product of covariates and
coefficients}

\item{svr_alpha_i}{string name of the alpha_i variable, individual specific
marginal-effects}

\item{svr_beta_i}{string name of the beta_i variable, relative preference
weight for each child}

\item{fl_N_agg}{float total resource avaible for allocation, if not specific,
sum by svr_N_i}

\item{fl_rho}{float preference for equality for the planner}

\item{svr_inpalc}{string variable name for newly generated input optimal
allocation, single word no dash}

\item{svr_expout}{string variable name for newly generated expected outcome,
single word no dash}
}
\value{
a dataframe that expands the df inputs with additional results.

a list with a dataframe and an array \itemize{ \item df_opti -
  Dataframe with various statistcs related to optimal allocation, all
  intermediary stats \item ar_opti_inpalc - Array where each element is an
  optimal choice for each individual \item ar_opti_expout - Array where each
  element is the expected outcome given optimal choices for each i }
}
\description{
This is the solution to the linear optimal allocation problem.
Relative allocations, summed splined, inversion. solin, solution linear.
relow, relative to the lowest allocation algoritm.
}
\examples{
data(df_opt_dtgch_cbem4)
df <- df_opt_dtgch_cbem4
svr_A_i <- 'A_lin'
svr_alpha_i <- 'alpha_lin'
svr_beta_i <- 'beta'
fl_N_agg <- 10000
fl_rho <- -30
ls_lin_solu <- ffp_opt_solin_relow(df, svr_A_i, svr_alpha_i, svr_beta_i, fl_N_agg, fl_rho)
df_opti <- ls_lin_solu$df_opti
ar_opti_inpalc <- ls_lin_solu$ar_opti_inpalc
ar_opti_expout <- ls_lin_solu$ar_opti_expout
summary(df_opti)
print(ar_opti_inpalc)
print(ar_opti_expout)
}
\references{
\url{https://fanwangecon.github.io/PrjOptiAlloc/reference/ffp_opt_solin_relow.html}
\url{https://fanwangecon.github.io/PrjOptiAlloc/articles/ffv_opt_solin_relow.html}
}
\author{
Fan Wang, \url{http://fanwangecon.github.io}
}
