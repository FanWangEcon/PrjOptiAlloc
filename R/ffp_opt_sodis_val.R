ffp_opt_sodis_value <- function(fl_rho, df_queue_il,
                                bl_return_allQ_V = FALSE,
                                bl_return_inner_V = FALSE,
                                svr_id_i = 'id_i',
                                svr_D_il = 'D_il', svr_inpalc = 'Q_il', svr_D_Wbin_il = 'D_Wbin_il',
                                svr_A_il = 'A_il', svr_alpha_il = 'alpha_il', svr_beta_i = 'beta_i',
                                svr_V_cumu_l = 'V_sum_l',
                                svr_V_inner_Q_il = 'V_inner_Q_il',
                                svr_V_star_Q_il = 'V_star_Q_il'){
#' Value at each W point along Queue, given optimal allocation
#'
#' @description
#' After solving for optimal allocating queue, at each Q point, which corresponds to
#' different level of aggregate resources available, show the value given optimal choices
#' this is an array of values, at each Q point. Potentially output for all Q. Or
#' only show this for all queue points up to the actual resource limit, D_Wbin_il = 1.
#'
#' @author Fan Wang, \url{http://fanwangecon.github.io}
#' @references
#' \url{https://fanwangecon.github.io/PrjOptiAlloc/reference/ffp_opt_sodis_value.html}
#' \url{https://fanwangecon.github.io/PrjOptiAlloc/articles/ffv_opt_sodis_rkone_casch_allrw.html}
#' @export
#'

  if(length(fl_rho)>1){
    # rho could be fed in an an array, with all identical values
    fl_rho <- fl_rho[1]
  }

  # A.1 Di=0 Utility for all
  df_rev_dizr_i_onerho <- df_queue_il %>%
      filter(!!sym(svr_D_il) == 1) %>%
      select(!!sym(svr_id_i), !!sym(svr_beta_i), !!sym(svr_A_il)) %>%
      mutate(!!sym(svr_V_cumu_l) := !!sym(svr_beta_i)*((!!sym(svr_A_il))^fl_rho),
             !!sym(svr_inpalc) := 0) %>%
      select(!!sym(svr_id_i), !!sym(svr_inpalc), !!sym(svr_V_cumu_l))

  # A.2 Cumulative Within Person Utility Inner Power Di, only D_Wbin_il == 1, those within allocaiton bound
  if (bl_return_allQ_V) {
    df_rev_il_long_onerho <- df_queue_il
  } else {
    # only evaluate up to resource
    df_rev_il_long_onerho <- df_queue_il %>% filter(!!sym(svr_D_Wbin_il) == 1)
  }
  df_rev_il_long_onerho <- df_rev_il_long_onerho %>%
                              mutate(!!sym(svr_V_cumu_l) :=
                                       !!sym(svr_beta_i)*((!!sym(svr_A_il)+!!sym(svr_alpha_il))^fl_rho)) %>%
                              select(!!sym(svr_id_i), !!sym(svr_inpalc), !!sym(svr_V_cumu_l))

  # A.3 Run cum sum function
  df_rev_il_long_onerho <- rbind(df_rev_dizr_i_onerho, df_rev_il_long_onerho)
  df_rev_il_long_onerho <- df_rev_il_long_onerho %>%
                              select(!!sym(svr_id_i), !!sym(svr_inpalc), !!sym(svr_V_cumu_l))
  df_rev_il_long_onerho <- ff_panel_cumsum_grouplast(df_rev_il_long_onerho,
                                                     svr_id=svr_id_i, svr_x=svr_inpalc, svr_y=svr_V_cumu_l,
                                                     svr_cumsumtop = svr_V_inner_Q_il,
                                                     stat='sum', quick=TRUE)

  # A.4 Outter power
  # Exclude Rank = 0, already used them to calculate total cumulative
  df_rev_il_long_onerho <- df_rev_il_long_onerho %>% filter(!!sym(svr_inpalc) != 0)
  df_rev_Ail_onerho <- df_rev_il_long_onerho %>%
    mutate(!!sym(svr_V_star_Q_il) := (!!sym(svr_V_inner_Q_il))^(1/fl_rho))


  # Export: function is given rho, so no rho to export
  svr_return_vars <- c(svr_inpalc, svr_V_star_Q_il)
  if (bl_return_inner_V) {
    svr_return_vars <- c(svr_inpalc, svr_V_cumu_l, svr_V_inner_Q_il, svr_V_star_Q_il)
  }
  df_rev_Ail_onerho <- df_rev_Ail_onerho %>% select(one_of(svr_return_vars))

  # Return
  return(df_rev_Ail_onerho)
}
