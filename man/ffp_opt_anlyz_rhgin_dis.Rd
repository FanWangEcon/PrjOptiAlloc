% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ffp_opt_sodis.R
\name{ffp_opt_anlyz_rhgin_dis}
\alias{ffp_opt_anlyz_rhgin_dis}
\title{Discrete Optimal Allocations, Queue, and Values}
\usage{
ffp_opt_anlyz_rhgin_dis(
  ar_rho,
  fl_teacher_increase_number,
  df_input_il,
  bl_df_alloc_il = FALSE,
  bl_return_V = TRUE,
  bl_return_allQ_V = FALSE,
  bl_return_inner_V = FALSE,
  svr_rho = "rho",
  svr_rho_val = "rho_val",
  svr_id_i = "id_i",
  svr_id_il = "id_il",
  svr_D_max_i = "D_max_i",
  svr_D_il = "D_il",
  svr_D_star_i = "D_star_i",
  svr_F_star_i = "F_star_i",
  svr_EH_star_i = "EH_star_i",
  svr_inpalc = "Q_il",
  svr_D_Wbin_il = "D_Wbin_il",
  svr_A_il = "A_il",
  svr_alpha_il = "alpha_il",
  svr_beta_i = "beta_i",
  svr_measure_i = NA,
  svr_mass_cumu_il = "mass_cumu_il",
  svr_expout = "opti_exp_outcome",
  svr_V_star_Q_il = "V_star_Q_il",
  st_idcol_prefix = "sid_"
)
}
\arguments{
\item{fl_teacher_increase_number}{is the amount of resources (in measure if
svr_measure_i is not NA) available for allocation.}

\item{bl_df_alloc_il}{boolean if true this will output a matrix where each
column is a different i individual and each row is a position along the
queue, and each cell is the level of allocation for individual i when the
overall allocation queue is up to the current queue position.}

\item{svr_beta_i}{string variable name for planner bias}

\item{svr_measure_i}{string variable name for mass for this type of
recipient, default NA mass of recipient is the measure of recipient of
this type in the population. This measure does not impact relative
ranking optimal allocation across types, but determines how much to push
individual types further along the allocation queue back.}
}
\description{
Optimal ALlocation Queues for Discrete Problems, Allocation
Amount, Value
}
\references{
\url{https://fanwangecon.github.io/PrjOptiAlloc/reference/ffp_opt_anlyz_rhgin_dis.html}
\url{https://fanwangecon.github.io/PrjOptiAlloc/articles/ffv_opt_sodis_rkone_casch_allrw.html}
}
\author{
Fan Wang, \url{http://fanwangecon.github.io}
}
