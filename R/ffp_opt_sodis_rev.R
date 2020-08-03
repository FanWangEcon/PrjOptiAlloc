ffp_opt_anlyz_sodis_rev <- function(ar_rho,
                                    it_w_agg,
                                    df_input_ib, df_queue_il_long_with_V,
                                    svr_rho = 'rho', svr_rho_val = 'rho_val',
                                    svr_A_i_l0 = 'A_i_l0', svr_alpha_o_i = 'alpha_o_i',
                                    svr_inpalc = 'Q_il',
                                    svr_beta_i = 'beta_i',
                                    svr_measure_i = NA, svr_mass_cumu_il = 'mass_cumu_il',
                                    svr_V_star_Q_il = 'V_star_Q_il'){
  #' Discrete Problem Resource Equivalent Variation Multiple Rhos
  #'
  #' @description
  #' Welfare distance is very easy to compute given analytical resource expansion path. For
  #' each planner preference, the solution does not generate optimal allocation for a particular level
  #' of aggregate resources, but generates queue of allocations that uniquely define allocation
  #' along the entire resource expansion. Because of this, it is possible to trivially compute value
  #' given optimal choices at each incremental point of the resourc expansion path.
  #'
  #' The value along the resource expansion path that is dependent on preference is stored
  #' inside the \strong{df_queue_il_long_with_V} dataframe's variable \emph{svr_V_star_Q_il}.
  #' Note in a normal problem the resource expansion path goes up to infinity, but given the upper
  #' bounds in the individual allocations, the resource expansion path is finite where the final
  #' point is equivalent to the sum of maximum allocations across individuals. In the resource
  #' expansion path dataframe \strong{df_queue_il_long_with_V}, additional variables needed are:
  #' \emph{svr_rho} and \emph{svr_rho_val} for the \eqn{\rho} key and value; \emph{svr_inpalc} which
  #' is the queue ranking number, but is also equivalent to the current aggregate resource level. If
  #' there are 2 individuals with in total at most 11 units of allocations, and the problem was solved
  #' at three different plann preference levels, this dataframe would have \eqn{11 \cdot 3} rows.
  #'
  #' On the othe rhand, we need from dataframe \strong{df_input_ib} information on alternative
  #' allocations. If there are two individuals, this dataframe would only have two rows. There are
  #' three variables needed: \emph{A_i_l0} for the needs at allocation equal to zero; \emph{alpha_o_i}
  #' for the effectiveness measured given cumulative observed allocation for each individual \eqn{i};
  #' and also needed for value calculation \emph{beta_i}. Note that these are the three ingredients
  #' that are individual specific.
  #'
  #' @param ar_rho array preferences for equality for the planner
  #' @param it_w_agg integer data/observed aggregate resources, \eqn{\hat{W}^{o}}.
  #' @param df_input_ib dataframe of \eqn{A_{0,i}} and \eqn{\alpha_{o,i}}, constructed based
  #' on individual \eqn{A} without allocation, and the cumulative aggregate effects of allocation
  #' given what is oboserved. The dataframe needs three variables, \eqn{A_{0,i}}, \eqn{\alpha_{o,i}}
  #' and \eqn{\beta_{i}}. Note that an ID variable is not needed. Because no merging is needed. Also
  #' note that \eqn{\rho} values are not needed because that will be supplied by \strong{df_queue_il_long_with_V}.
  #' @param df_queue_il_long_with_V dataframe with optimal allocation resource expansion results,
  #' including the value along resource expansion so that observed value can be compared to.
  #' @param svr_A_i_l0 string variable name in the \strong{df_input_ib} dataframe for \eqn{A_{0,i}}.
  #' @param svr_alpha_o_i string variable name in the \strong{df_input_ib} dataframe for \eqn{\alpha_{o,i}}.
  #' @param svr_alpha_o_i string variable name in the \strong{df_input_ib} dataframe for \eqn{\alpha_{o,i}}.
  #' @author Fan Wang, \url{http://fanwangecon.github.io}
  #' @references
  #' \url{https://fanwangecon.github.io/PrjOptiAlloc/reference/ffp_opt_anlyz_sodis_rev.html}
  #' \url{https://fanwangecon.github.io/PrjOptiAlloc/articles/ffv_opt_sodis_rkone_casch_allrw.html}
  #' @export
  #' @examples
  #' data(df_opt_caschool_input_ib)
  #' df_input_ib <- df_opt_caschool_input_ib

  # Evaluate REV
  ar_util_rev_loop <- df_queue_il_long_with_V %>%
    group_by(!!sym(svr_rho)) %>%
    do(rev = ffp_opt_sodis_rev(fl_rho = .[[svr_rho_val]],
                               it_w_agg = it_w_agg,
                               df_input_ib = df_input_ib, df_queue_il_with_V = .,
                               svr_A_i_l0 = svr_A_i_l0, svr_alpha_o_i = svr_alpha_o_i,
                               svr_inpalc = svr_inpalc,
                               svr_beta_i = svr_beta_i,
                               svr_measure_i = svr_measure_i, svr_mass_cumu_il = svr_mass_cumu_il,
                               svr_V_star_Q_il = svr_V_star_Q_il)$fl_REV) %>%
    unnest() %>% pull()

  # Return Matrix
  mt_rho_rev <- cbind(ar_rho, ar_util_rev_loop)
  colnames(mt_rho_rev) <- c(svr_rho_val,'REV')
  tb_rho_rev <- as_tibble(mt_rho_rev) %>% rowid_to_column(var = svr_rho)

  # Retrun
  return(tb_rho_rev)
}


ffp_opt_sodis_rev <- function(fl_rho,
                              it_w_agg,
                              df_input_ib, df_queue_il_with_V,
                              svr_A_i_l0 = 'A_i_l0', svr_alpha_o_i = 'alpha_o_i',
                              svr_inpalc = 'Q_il',
                              svr_beta_i = 'beta_i',
                              svr_measure_i = NA, svr_mass_cumu_il = 'mass_cumu_il',
                              svr_V_star_Q_il = 'V_star_Q_il'){
  #' Discrete Problem Resource Equivalent Variation
  #'
  #' @description
  #' The function uses the Value already computed along the Queue
  #'
  #' @author Fan Wang, \url{http://fanwangecon.github.io}
  #' @references
  #' \url{https://fanwangecon.github.io/PrjOptiAlloc/reference/ffp_opt_sodis_rev.html}
  #' \url{https://fanwangecon.github.io/PrjOptiAlloc/articles/ffv_opt_sodis_rkone_casch_allrw.html}
  #' @export
  #'

  if(length(fl_rho)>1){
    # rho could be fed in an an array, with all identical values
    fl_rho <- fl_rho[1]
  }

  # A. Bias and Mass
  if (is.na(svr_measure_i)) {
    # do not modify beta
    df_input_ib <- df_input_ib %>%
      mutate(bias_weight = (!!sym(svr_beta_i)) )
  } else {
    # Update the weight column so that weight considers both mass and bias
    df_input_ib <- df_input_ib %>%
      mutate(bias_weight = (!!sym(svr_beta_i)*!!sym(svr_measure_i)) )
  }

  # B. Aggregate utility given Alternative Allocation
  fl_util_alter_alloc <- df_input_ib %>%
    mutate(v_altern_i = bias_weight*((!!sym(svr_A_i_l0) + !!sym(svr_alpha_o_i))^fl_rho)) %>%
    summarize(v_altern_unif_i = sum(v_altern_i)^(1/fl_rho)) %>%
    pull()

  # C. Generate rho specific REV
  if (is.na(svr_measure_i)) {
    svr_mass_or_queue <- svr_inpalc
  } else {
    # see ffp_opt_anlyz_rhgin_dis-L205
    svr_mass_or_queue <- svr_mass_cumu_il
  }

  it_w_exp_min <- min(df_queue_il_with_V %>%
                        filter(!!sym(svr_V_star_Q_il) >= fl_util_alter_alloc) %>%
                        pull(!!sym(svr_mass_or_queue)))

  # D. There are some exceptions:
  # it could be that when optimal choice is observed, value are almost so identical yet slighlty not
  # this leads to empty it_w_exp_min, no value selectec, no allocation gives better than Observed
  if (it_w_exp_min == Inf) {
    it_w_exp_min = it_w_agg
  }

  # E. for the same reason as D, minor approximation error when observed choice is optimal.
  # for both E and D, the df_input_ib matrix does not store allocation but A and alpha given
  # allocations, so it is not possible to directly condition for cases where observed is optimal
  if (it_w_exp_min > it_w_agg) {
    it_w_exp_min = it_w_agg
  }


  fl_REV <- 1 - (it_w_exp_min/it_w_agg)

  # Return
  return(list(it_w_exp_min=it_w_exp_min,
              fl_REV=fl_REV))
}
