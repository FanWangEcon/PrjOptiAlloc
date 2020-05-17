ffd_draw_n_alpha <- function(fl_w_dollar = 100,
                             it_w_units_avg = 5,
                             fl_eh_max_inc_i = 5,
                             fl_min_max_eh_inc_ratio = 1.5,
                             fl_disc_cts_match_ratio = 1.1,
                             fl_A_i_alpha_i1_ratio = 2,
                             fl_A_i_relative_ratio = 3,
                             it_N = 2,
                             it_rand_seed = 123){
  #' Generate N=2 Data for Discrete and Bounded Continuous Examples
  #'
  #' @description
  #' Generates arbitrary dataframes needed for discrete as well as continuous optimization problems when there are N individuals?
  #'
  #' @param fl_w_dollar float amount of aggregate resources in dollar for both individuals
  #' @param it_w_units_avg float number of goods for cts at max, approxi number for dis
  #' @param fl_eh_max_inc_i float maximum increase EH for one individual
  #' @param fl_min_max_eh_inc_ratio float ratio of maximum and minimum outcome increase
  #' @param fl_disc_cts_match_ratio float if 1, disc unit = it_w_units_avg
  #' @param fl_A_i_alpha_i1_ratio float A_i divide the alphi_il ratio, discrete set this
  #' @param fl_A_i_relative_ratio float Relative ratio of A between the two individuals
  #' @param it_N integer the number of individuals
  #' @param it_rand_seed integer random seed

  set.seed(it_rand_seed)

  # Let price be the same, does not matter, only marginal effect over price ratio matters
  fl_price <- fl_w_dollar/it_w_units_avg
  fl_cts_alpha_max <- ((fl_eh_max_inc_i*fl_price)/fl_w_dollar)
  fl_cts_alpha_min <- fl_cts_alpha_max/fl_min_max_eh_inc_ratio

  # Generate two individuals at min and max, otherwise draw randomly between them as well
  if (it_N == 1) {
    stop("Need to draw for at least two individuals")
  } else {
    ar_cts_alpha <- c(fl_cts_alpha_min, fl_cts_alpha_max)
    if (it_N > 2) {
      ar_cts_alpha_more <-
        runif((it_N-2), min=fl_cts_alpha_min, max=fl_cts_alpha_max)
      ar_cts_alpha <- c(ar_cts_alpha, ar_cts_alpha_more)
    }
    ar_cts_alpha <- sort(ar_cts_alpha, decreasing=TRUE)
  }

  # Aggregate W for Discrete
  ar_A_i_sum <- fl_w_dollar*(ar_cts_alpha/fl_price)

  # Draw A_il
  ar_it_w_units <- sample(round(
    seq(it_w_units_avg/fl_disc_cts_match_ratio,
        it_w_units_avg*fl_disc_cts_match_ratio, length.out=it_N)),
    it_N, replace=TRUE)

  # Genereate alpha_il
  ls_ar_alpha_il = apply(
    cbind(ar_it_w_units, ar_A_i_sum),
    1,
    function(row, min, max) {
      it_draw <- row[1]
      fl_sum <- row[2]
      ar_unif <- runif(it_draw,
                       min=fl_cts_alpha_min,
                       max=fl_cts_alpha_max)
      ar_unif <- sort(ar_unif, decreasing=TRUE)
      ar_share <- ar_unif/sum(ar_unif)
      ar_levels <- (ar_share*fl_sum*fl_disc_cts_match_ratio)
      ar_share <- ar_share[ar_levels < fl_sum]
      ar_levels <- ar_levels[ar_levels < fl_sum]
      return(list(ar_share=ar_share,
                  ar_levels=ar_levels))
    })

  # Print All
  message(ls_ar_alpha_il)
  message(fl_w_dollar/fl_price)
  sapply(seq(it_N), function(x) sum(ls_ar_alpha_il[[x]]$ar_levels))
  message((fl_w_dollar*ar_cts_alpha)/fl_price)

  # Generate A_i that are consistent with alpha_i scales
  # randomly draw two individual N2 where the differences is 2x of A_il
  # Average of alpha_i1 of all individuals
  fl_alpha_i1_avg = mean(sapply(
    seq(it_N), function(x) ls_ar_alpha_il[[x]]$ar_levels[1]))
  # Average of A_i
  # C:\Users\fan\R4Econ\math\solutions\fs_solu_x_lin.Rmd
  fl_a <- fl_A_i_relative_ratio
  fl_b <- fl_alpha_i1_avg
  fl_A_i_gap = (fl_b*(fl_a - 1))/(1 + fl_a)
  ar_A_i = runif(it_N,min=fl_b - fl_A_i_gap, max=fl_b + fl_A_i_gap)

  # return otuputs
  return(list(ar_A_i = ar_A_i,
              ls_ar_alpha_il=ls_ar_alpha_il,
              fl_price = fl_price,
              ar_cts_alpha = ar_cts_alpha,
              it_rand_seed=it_rand_seed))
}
