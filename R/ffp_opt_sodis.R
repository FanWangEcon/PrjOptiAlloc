ffp_opt_sodis_value <- function(fl_rho, df_queue_il,
                                bl_return_allQ_V = FALSE,
                                bl_return_inner_V = FALSE,
                                svr_id_i = 'id_i',
                                svr_D_il = 'D_il', svr_inpalc = 'Q_il', svr_D_Wbin_il = 'D_Wbin_il',
                                svr_A_il = 'A_il', svr_alpha_il = 'alpha_il', svr_beta_i = 'beta_i',
                                svr_V_cumu_l = 'V_sum_l',
                                svr_V_inner_Q_il = 'V_inner_Q_il',
                                svr_V_star_Q_il = 'V_star_Q_il'){

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


ffp_opt_anlyz_rhgin_dis <- function(ar_rho,
                                    fl_teacher_increase_number,
                                    df_input_il,
                                    bl_df_alloc_il = FALSE,
                                    bl_return_V = TRUE,
                                    bl_return_allQ_V = FALSE,
                                    bl_return_inner_V = FALSE,
                                    svr_rho = 'rho', svr_rho_val = 'rho_val',
                                    svr_id_i = 'id_i', svr_id_il = 'id_il',
                                    svr_D_max_i = 'D_max_i', svr_D_il = 'D_il',
                                    svr_D_star_i = 'D_star_i', svr_F_star_i = 'F_star_i', svr_EH_star_i = 'EH_star_i',
                                    svr_inpalc = 'Q_il', svr_D_Wbin_il = 'D_Wbin_il',
                                    svr_A_il = 'A_il', svr_alpha_il = 'alpha_il', svr_beta_i = 'beta_i',
                                    svr_expout = 'opti_exp_outcome',
                                    svr_V_star_Q_il = 'V_star_Q_il',
                                    st_idcol_prefix = 'sid_'){

    # Step 1
    # Call function to Solve for Optimal Targeting Queue
    ls_df_queues <- ffp_opt_anlyz_rhgin_bin(df_input_il,
                                            svr_id_i = svr_id_il,
                                            svr_A_i = svr_A_il, svr_alpha_i = svr_alpha_il, svr_beta_i = svr_beta_i,
                                            ar_rho = ar_rho, svr_rho_val = svr_rho_val,
                                            svr_rho = svr_rho,
                                            svr_inpalc = svr_inpalc,
                                            svr_expout = svr_expout,
                                            verbose = FALSE)
    df_queue_il_long <- ls_df_queues$df_all_rho_long
    df_queue_il_wide <- ls_df_queues$df_all_rho

    # Step 2
    # Allocations that would be below resource threshold.
    df_queue_il_long <- df_queue_il_long %>%
      mutate(!!sym(svr_D_Wbin_il) :=
               case_when(!!sym(svr_inpalc) <= fl_teacher_increase_number ~ 1,
                         TRUE ~ 0)) %>%
      left_join(df_input_il %>%
                  select(!!sym(svr_id_i), !!sym(svr_id_il),
                         !!sym(svr_D_max_i), !!sym(svr_D_il)),
                by=svr_id_il)

    # Step 3
    # Optimal Allocation Discrete Levels
    df_alloc_i_long <- df_queue_il_long %>%
      select(!!sym(svr_rho), !!sym(svr_rho_val), !!sym(svr_id_i),
             !!sym(svr_D_max_i), !!sym(svr_D_Wbin_il)) %>%
      group_by(!!sym(svr_id_i), !!sym(svr_rho)) %>%
      summarize(!!sym(svr_rho_val)  := mean(!!sym(svr_rho_val)),
                !!sym(svr_D_max_i)  := mean(!!sym(svr_D_max_i)),
                !!sym(svr_D_star_i) := sum(!!sym(svr_D_Wbin_il)),
                !!sym(svr_F_star_i) := (!!sym(svr_D_star_i)/!!sym(svr_D_max_i))) %>%
      arrange(!!sym(svr_id_i), !!sym(svr_rho))

    # Step 4
    # Expected Outcome Given Optimal Choices
    # df_alloc_i_long -> left_join(df_input_il) -> Generate EH
    # Very straight forward, except need to deal with D_star_i = 0, merge D_star_i and di jointly
    df_alloc_i_long <- df_alloc_i_long %>%
      mutate(D_star_i_zr1 =
               case_when(!!sym(svr_D_star_i) == 0 ~ 1, # for merging where D_star = 0
                         TRUE ~ !!sym(svr_D_star_i))) %>%
      left_join(df_input_il %>%
                  select(!!sym(svr_id_i), !!sym(svr_D_il),
                         !!sym(svr_A_il), !!sym(svr_alpha_il)),
                by=setNames(c(svr_id_i, svr_D_il), c(svr_id_i, 'D_star_i_zr1'))) %>%
      mutate(!!sym(svr_EH_star_i) :=
               case_when(!!sym(svr_D_star_i) == 0 ~ !!sym(svr_A_il), # no choice no allocation
                         TRUE ~ !!sym(svr_A_il) + !!sym(svr_alpha_il)))
    # Step 7
    # Gini Calculations
    ar_gini_D_star <- df_alloc_i_long %>% select(one_of(svr_rho, svr_D_star_i)) %>%
      group_by(!!sym(svr_rho)) %>%
      do(D_star_gini = ff_dist_gini_vector_pos(.[[svr_D_star_i]])) %>%
      unnest(c(D_star_gini)) %>% pull()
    ar_gini_EH_star <- df_alloc_i_long %>% select(one_of(svr_rho, svr_EH_star_i)) %>%
      group_by(!!sym(svr_rho)) %>%
        do(EH_star_gini = ff_dist_gini_vector_pos(.[[svr_EH_star_i]])) %>%
        unnest(c(EH_star_gini)) %>% pull()
    mt_rho_gini <- cbind(ar_rho, ar_gini_D_star, ar_gini_EH_star)
    colnames(mt_rho_gini) <- c(svr_rho_val, 'gini_D_star', 'gini_EH_star')
    df_rho_gini <- as_tibble(mt_rho_gini) %>% rowid_to_column(var = svr_rho)

    # Step 5a, value at Q points
    svr_return_list <- c(svr_rho, svr_rho_val, svr_id_i, svr_id_il,
                         svr_D_max_i, svr_D_il, svr_inpalc, svr_D_Wbin_il)
    # svr_A_il, svr_alpha_il, svr_beta_i
    if (bl_return_V){
      mt_util_rev_loop <- df_queue_il_long %>%
        group_by(!!sym(svr_rho)) %>%
        do(rev = ffp_opt_sodis_value(fl_rho = .[[svr_rho_val]],
                                       df_queue_il = .,
                                       bl_return_allQ_V = bl_return_allQ_V,
                                       bl_return_inner_V = bl_return_inner_V,
                                       svr_id_i = svr_id_i,
                                       svr_D_il = svr_D_il, svr_inpalc = svr_inpalc, svr_D_Wbin_il = svr_D_Wbin_il,
                                       svr_A_il = svr_A_il, svr_alpha_il = svr_alpha_il, svr_beta_i = svr_beta_i,
                                       svr_V_star_Q_il = svr_V_star_Q_il)) %>%
            unnest()
      # Step 5b, merge values to queue df
      df_queue_il_long <- df_queue_il_long %>% left_join(mt_util_rev_loop,
                                     by=setNames(c(svr_rho, svr_inpalc), c(svr_rho, svr_inpalc))) %>%
                          select(one_of(svr_return_list), starts_with('V_'))
    } else {
      # Step 5c, no value return
      df_queue_il_long <- df_queue_il_long %>% select(one_of(svr_return_list))
    }

    # Step 6
    # Sort columns
    df_alloc_i_long <- df_alloc_i_long %>%
      select(!!sym(svr_rho), !!sym(svr_rho_val),
             !!sym(svr_id_i),
             !!sym(svr_D_max_i), !!sym(svr_D_star_i), !!sym(svr_F_star_i), !!sym(svr_EH_star_i))


     # Return List Main
     ls_return <- list(df_queue_il_long=df_queue_il_long,
                       df_queue_il_wide=df_queue_il_wide,
                       df_alloc_i_long=df_alloc_i_long,
                       df_rho_gini=df_rho_gini)

     # Alloc IL, allocation for each individual up to Qth queue position
     if (bl_df_alloc_il) {

       df_alloc_il_long <- df_queue_il_long %>%
         select(one_of(svr_rho, svr_id_i, svr_inpalc)) %>%
         group_by(!!sym(svr_rho)) %>%
         do(alloc_i_upto_Q =
              ff_panel_expand_longrosterwide(df=.,
                                             svr_id_t=svr_inpalc,
                                             svr_id_i=svr_id_i,
                                             st_idcol_prefix=st_idcol_prefix)$df_roster_wide_cumu) %>%
         unnest() %>% select(-one_of(paste0('rho', '1')))

       # Append additional return list element
       ls_return$df_alloc_il_long <- df_alloc_il_long
     }

     # Return
     return(ls_return)

}

# ffp_opt_anlyz_rhgin_dis <- function(ar_rho,
#                                     it_w_agg,
#                                     df_input_il,
#                                     svr_rho = 'rho',
#                                     svr_id_i = 'id_i', svr_id_il = 'id_il',
#                                     svr_D_max_i = 'D_max_i', svr_D_il = 'D_il',
#                                     svr_D_star_i = 'D_star_i', svr_F_star_i = 'F_star_i',
#                                     svr_inpalc = 'Q_il', svr_D_Wbin_il = 'D_Wbin_il',
#                                     svr_A_il = 'A_il', svr_alpha_il = 'alpha_il', svr_beta_i = 'beta_i',
#                                     svr_expout = 'opti_exp_outcome',
#                                     st_idcol_prefix = 'sid_',
#                                     bl_df_alloc_il = FALSE){
# #' Discrete Optimal Allocation Solution, Common Price, Jointly Solve Over Many Rhos
# #'
# #' @description
# #' There are N individuals, and each could be allocated some units of allocation, each
# #' unit of additional allocation has non-increasing effects. We have multiple planner preference lambda
# #' values as well. Solve for optimal targeting queue and allocation levels for each individual
# #' given heterogeneous effects of each incremental allocation, and the level of outcome individual would
# #' reach without each unit of allocation. Optimal allocation level determined by it_w_agg, the aggregate
# #' level of resource available. The problem is only interesting if the total number of potential slots
# #' to be receiving allocation exceeds the aggregate resource available.
#
#     # Call function to Solve for Optimal Targeting Queue
#     ls_df_queues <- ffp_opt_anlyz_rhgin_bin(df_input_il,
#                                             svr_id_i = svr_id_il,
#                                             svr_A_i = svr_A_il, svr_alpha_i = svr_alpha_il, svr_beta_i = svr_beta_i,
#                                             ar_rho = ar_rho,
#                                             svr_rho = svr_rho,
#                                             svr_inpalc = svr_inpalc,
#                                             svr_expout = svr_expout,
#                                             verbose = FALSE)
#
#     # Allocations that would be below resource threshold.
#     df_queue_il_long <- ls_df_queues$df_all_rho_long %>%
#       mutate(!!sym(svr_D_Wbin_il) :=
#                case_when(!!sym(svr_inpalc) <= it_w_agg ~ 1,
#                          TRUE ~ 0)) %>%
#       left_join(df_input_il %>%
#                   select(!!sym(svr_id_i), !!sym(svr_id_il),
#                          !!sym(svr_D_max_i), !!sym(svr_D_il)),
#                 by=svr_id_il) %>%
#       select(!!sym(svr_rho),
#              !!sym(svr_id_i), !!sym(svr_id_il),
#              !!sym(svr_D_max_i), !!sym(svr_D_il),
#              !!sym(svr_inpalc), !!sym(svr_D_Wbin_il),
#              !!sym(svr_A_il), !!sym(svr_alpha_il), !!sym(svr_beta_i))
#
#     # Results from optimal targeting
#     # wide frame is mainly for visualization and analysis
#     df_queue_il_wide <- ls_df_queues$df_all_rho_long
#
#     # Optimal ALlocations, Aggregating
#     df_alloc_i_long <- df_queue_il_long %>%
#       select(!!sym(svr_rho), !!sym(svr_id_i),
#              !!sym(svr_D_max_i), !!sym(svr_D_Wbin_il)) %>%
#       group_by(!!sym(svr_id_i), !!sym(svr_rho)) %>%
#       summarize(!!sym(svr_D_max_i)  := mean(!!sym(svr_D_max_i)),
#                 !!sym(svr_D_star_i) := sum(!!sym(svr_D_Wbin_il)),
#                 !!sym(svr_F_star_i) := (!!sym(svr_D_star_i)/!!sym(svr_D_max_i))) %>%
#       mutate(!!sym(svr_rho) := as.numeric(!!sym(svr_rho))) %>%
#       arrange(!!sym(svr_id_i), !!sym(svr_rho))
#
#     # Return List Main
#     ls_return <- list(df_queue_il_long=df_queue_il_long,
#                       df_queue_il_wide=df_queue_il_wide,
#                       df_alloc_i_long=df_alloc_i_long)
#
#     # Alloc IL, allocation for each individual up to Qth queue position
#     if (bl_df_alloc_il) {
#
#       df_alloc_il_long <- df_queue_il_long %>%
#         select(one_of(svr_rho, svr_id_i, svr_inpalc)) %>%
#         group_by(!!sym(svr_rho)) %>%
#         do(alloc_i_upto_Q =
#              ff_panel_expand_longrosterwide(df=.,
#                                             svr_id_t=svr_inpalc,
#                                             svr_id_i=svr_id_i,
#                                             st_idcol_prefix=st_idcol_prefix)$df_roster_wide_cumu) %>%
#         unnest() %>% select(-one_of(paste0('rho', '1')))
#
#       # Append additional return list element
#       ls_return$df_alloc_il_long <- df_alloc_il_long
#     }
#
#     # Return
#     return(ls_return)
# }

ffp_opt_sodis_rev <- function(fl_rho,
                                it_w_agg,
                                df_input_ib, df_queue_il_with_V,
                                svr_A_i_l0 = 'A_i_l0', svr_alpha_o_i = 'alpha_o_i',
                                svr_inpalc = 'Q_il',
                                svr_beta_i = 'beta_i',
                                svr_V_star_Q_il = 'V_star_Q_il'){
  if(length(fl_rho)>1){
    # rho could be fed in an an array, with all identical values
    fl_rho <- fl_rho[1]
  }

  # B. Aggregate utility given Alternative Allocation
  fl_util_alter_alloc <- df_input_ib %>%
      mutate(v_altern_i = !!sym(svr_beta_i)*((!!sym(svr_A_i_l0) + !!sym(svr_alpha_o_i))^fl_rho)) %>%
      summarize(v_altern_unif_i = sum(v_altern_i)^(1/fl_rho)) %>%
      pull()

  # C. Generate rho specific REV
  it_w_exp_min <- min(df_queue_il_with_V %>%
                        filter(!!sym(svr_V_star_Q_il) >= fl_util_alter_alloc) %>%
                        pull(!!sym(svr_inpalc)))
  fl_REV <- 1 - (it_w_exp_min/it_w_agg)

  # Return
  return(list(it_w_exp_min=it_w_exp_min,
              fl_REV=fl_REV))
}

# ffp_opt_sodis_rev <- function(fl_rho,
#                               it_w_agg,
#                               df_input_ib, df_queue_il,
#                               svr_A_i_l0 = 'A_i_l0', svr_alpha_o_i = 'alpha_o_i',
#                               svr_id_i = 'id_i',
#                               svr_D_il = 'D_il', svr_inpalc = 'Q_il', svr_D_Wbin_il = 'D_Wbin_il',
#                               svr_A_il = 'A_il', svr_alpha_il = 'alpha_il', svr_beta_i = 'beta_i',
#                               svr_v_cumu_l = 'v_sum_l',
#                               svr_v_cumu_il_rksrt = 'v_sum_il_rk',
#                               svr_v_cumu_il_rksrt_outter = 'v_sum_il_rk_out'){
# #' Optimal Utility at Each Aggregate Allocation Level and Resource Equivalent Variations
# #'
# #' @description
# #' Given the solutions to the optimal allocation problem, produced by \code{\link{ffp_opt_anlyz_rhgin_dis}},
# #' solves for multiple lambda valuesand given some set of alternative allocation available
# #' compute two things. First, at each incremental level of additional resource, given optimal allocation
# #' following the targeting queue, what is the planner value. This is the Value given optimal choice given
# #' resource constraint. Second, given the value achieved, following expenditure minimization, solve for the resource
# #' needed to achieve the same level of utility as some alternative allocation, which could be uniform, random
# #' or simply based on what is observed.
# #'
#
#   if(length(fl_rho)>1){
#     # rho could be fed in an an array, with all identical values
#     fl_rho <- fl_rho[1]
#   }
#
#   # A.1 Di=0 Utility for all
#   df_rev_dizr_i_onerho <- df_queue_il %>%
#       filter(!!sym(svr_D_il) == 1) %>%
#       select(!!sym(svr_id_i), !!sym(svr_beta_i), !!sym(svr_A_il)) %>%
#       mutate(!!sym(svr_v_cumu_l) := !!sym(svr_beta_i)*((!!sym(svr_A_il))^fl_rho),
#              !!sym(svr_inpalc) := 0) %>%
#       select(!!sym(svr_id_i), !!sym(svr_inpalc), !!sym(svr_v_cumu_l))
#
#   # A.2 Cumulative Within Person Utility Inner Power Di, only D_Wbin_il == 1, those within allocaiton bound
#   df_rev_il_long_onerho <- df_queue_il %>%
#                               filter(!!sym(svr_D_Wbin_il) == 1) %>%
#                               mutate(!!sym(svr_v_cumu_l) :=
#                                        !!sym(svr_beta_i)*((!!sym(svr_A_il)+!!sym(svr_alpha_il))^fl_rho)) %>%
#                               select(!!sym(svr_id_i), !!sym(svr_inpalc), !!sym(svr_v_cumu_l))
#
#   # A.3 Run cum sum function
#   df_rev_il_long_onerho <- rbind(df_rev_dizr_i_onerho, df_rev_il_long_onerho)
#   df_rev_il_long_onerho <- df_rev_il_long_onerho %>%
#                               select(!!sym(svr_id_i), !!sym(svr_inpalc), !!sym(svr_v_cumu_l))
#   df_rev_il_long_onerho <- ff_panel_cumsum_grouplast(df_rev_il_long_onerho,
#                                                      svr_id=svr_id_i, svr_x=svr_inpalc, svr_y=svr_v_cumu_l,
#                                                      svr_cumsumtop = svr_v_cumu_il_rksrt,
#                                                      stat='sum', quick=TRUE)
#
#   # A.4 Outter power
#   # Exclude Rank = 0, already used them to calculate total cumulative
#   df_rev_il_long_onerho <- df_rev_il_long_onerho %>% filter(!!sym(svr_inpalc) != 0)
#   df_rev_Ail_onerho <- df_rev_il_long_onerho %>%
#     mutate(!!sym(svr_v_cumu_il_rksrt_outter) := (!!sym(svr_v_cumu_il_rksrt))^(1/fl_rho))
#
#   # B. Aggregate utility given Alternative Allocation
#   fl_util_alter_alloc <- df_input_ib %>%
#       mutate(v_unif_i = !!sym(svr_beta_i)*((!!sym(svr_A_i_l0) + !!sym(svr_alpha_o_i))^fl_rho)) %>%
#       summarize(v_sum_unif_i = sum(v_unif_i)^(1/fl_rho)) %>%
#       pull()
#
#   # C. Generate rho specific REV
#   it_w_exp_min <- min(df_rev_Ail_onerho %>%
#                         filter(!!sym(svr_v_cumu_il_rksrt_outter) >= fl_util_alter_alloc) %>%
#                         pull(!!sym(svr_inpalc)))
#   fl_REV <- 1 - (it_w_exp_min/it_w_agg)
#
#   # Return
#   return(list(df_rev_Ail_onerho=df_rev_Ail_onerho,
#               it_w_exp_min=it_w_exp_min,
#               fl_REV=fl_REV))
# }


# ffp_opt_anlyz_sodis_rev <- function(ar_rho,
#                                     it_w_agg,
#                                     df_input_ib, df_queue_il_long,
#                                     svr_rho = 'rho',
#                                     svr_A_i_l0 = 'A_i_l0', svr_alpha_o_i = 'alpha_o_i',
#                                     svr_id_i = 'id_i',
#                                     svr_D_il = 'D_il', svr_inpalc = 'Q_il', svr_D_Wbin_il = 'D_Wbin_il',
#                                     svr_A_il = 'A_il', svr_alpha_il = 'alpha_il', svr_beta_i = 'beta_i',
#                                     svr_v_cumu_l = 'v_sum_l',
#                                     svr_v_cumu_il_rksrt = 'v_sum_il_rk',
#                                     svr_v_cumu_il_rksrt_outter = 'v_sum_il_rk_out'){
# #' Resource Equivalent Variation Across Inequality Aversion Parameter Values
# #'
# #' @description
# #' This is a wrapper for \code{\link{ffp_opt_sodis_rev}}, solves for multiple lambda values
# #'
#
#     # add rho value to queue_il_long, previously only key
#     df_queue_il_long_lamval <- df_queue_il_long %>%
#       mutate(!!sym(svr_rho) := as.numeric(!!sym(svr_rho))) %>%
#       left_join((as_tibble(ar_rho) %>%
#                    rename(rho_val = value) %>%
#                    rowid_to_column(var = svr_rho)),
#                 by=svr_rho)
#
#     # Evaluate REV
#     start_time_nest <- Sys.time()
#     ar_util_rev_loop <- df_queue_il_long_lamval %>%
#       group_by(!!sym(svr_rho)) %>%
#       do(rev = ffp_opt_sodis_rev(fl_rho = .$rho_val,
#                                  it_w_agg = it_w_agg,
#                                  df_input_ib = df_input_ib, df_queue_il = .,
#                                  svr_A_i_l0 = svr_A_i_l0, svr_alpha_o_i = svr_alpha_o_i,
#                                  svr_id_i = svr_id_i,
#                                  svr_D_il = svr_D_il, svr_inpalc = svr_inpalc, svr_D_Wbin_il = svr_D_Wbin_il,
#                                  svr_A_il = svr_A_il, svr_alpha_il = svr_alpha_il, svr_beta_i = svr_beta_i,
#                                  svr_v_cumu_l = svr_v_cumu_l,
#                                  svr_v_cumu_il_rksrt = svr_v_cumu_il_rksrt,
#                                  svr_v_cumu_il_rksrt_outter = svr_v_cumu_il_rksrt_outter)$fl_REV) %>%
#       unnest()
#
#   # Retrun
#   return(ar_util_rev_loop)
# }


ffp_opt_anlyz_sodis_rev <- function(ar_rho,
                                    it_w_agg,
                                    df_input_ib, df_queue_il_long_with_V,
                                    svr_rho = 'rho', svr_rho_val = 'rho_val',
                                    svr_A_i_l0 = 'A_i_l0', svr_alpha_o_i = 'alpha_o_i',
                                    svr_inpalc = 'Q_il',
                                    svr_beta_i = 'beta_i',
                                    svr_V_star_Q_il = 'V_star_Q_il'){

    # Evaluate REV
    ar_util_rev_loop <- df_queue_il_long_with_V %>%
      group_by(!!sym(svr_rho)) %>%
      do(rev = ffp_opt_sodis_rev(fl_rho = .[[svr_rho_val]],
                                   it_w_agg = it_w_agg,
                                   df_input_ib = df_input_ib, df_queue_il_with_V = .,
                                   svr_A_i_l0 = svr_A_i_l0, svr_alpha_o_i = svr_alpha_o_i,
                                   svr_inpalc = svr_inpalc,
                                   svr_beta_i = svr_beta_i,
                                   svr_V_star_Q_il = svr_V_star_Q_il)$fl_REV) %>%
      unnest() %>% pull()

    # Return Matrix
    mt_rho_rev <- cbind(ar_rho, ar_util_rev_loop)
    colnames(mt_rho_rev) <- c(svr_rho_val,'REV')
    tb_rho_rev <- as_tibble(mt_rho_rev) %>% rowid_to_column(var = svr_rho)

# Retrun
return(tb_rho_rev)
}
