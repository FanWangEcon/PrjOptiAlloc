% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ffd_opt_datagen.R
\name{ffd_draw_n_alpha}
\alias{ffd_draw_n_alpha}
\title{Generate N=2 Data for Discrete and Bounded Continuous Examples}
\usage{
ffd_draw_n_alpha(
  fl_w_dollar = 100,
  it_w_units_avg = 5,
  fl_eh_max_inc_i = 5,
  fl_min_max_eh_inc_ratio = 1.5,
  fl_disc_cts_match_ratio = 1.1,
  fl_A_i_alpha_i1_ratio = 2,
  fl_A_i_relative_ratio = 3,
  it_N = 2,
  it_rand_seed = 123
)
}
\arguments{
\item{fl_w_dollar}{float amount of aggregate resources in dollar for both individuals}

\item{it_w_units_avg}{float number of goods for cts at max, approxi number for dis}

\item{fl_eh_max_inc_i}{float maximum increase EH for one individual}

\item{fl_min_max_eh_inc_ratio}{float ratio of maximum and minimum outcome increase}

\item{fl_disc_cts_match_ratio}{float if 1, disc unit = it_w_units_avg}

\item{fl_A_i_alpha_i1_ratio}{float A_i divide the alphi_il ratio, discrete set this}

\item{fl_A_i_relative_ratio}{float Relative ratio of A between the two individuals}

\item{it_N}{integer the number of individuals}

\item{it_rand_seed}{integer random seed}
}
\description{
Generates arbitrary dataframes needed for discrete as well as continuous optimization problems when there are N individuals?
}
\author{
Fan Wang, \url{http://fanwangecon.github.io}
}
