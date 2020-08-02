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
  #' Discrete Optimal Allocations, Queue, and Values
  #'
  #' @description
  #' Optimal ALlocation Queues for Discrete Problems, Allocation Amount, Value
  #'
  #' @author Fan Wang, \url{http://fanwangecon.github.io}
  #' @references
  #' \url{https://fanwangecon.github.io/PrjOptiAlloc/reference/ffp_opt_anlyz_rhgin_dis.html}
  #' \url{https://fanwangecon.github.io/PrjOptiAlloc/articles/ffv_opt_sodis_rkone_casch_allrw.html}
  #' @export
  #'

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

    # Step 7 Aggregate Statistics: EH_star_mean = EH_star_i_mean
    svr_D_star_i_demean <- 'EH_star_i_demean'
    df_alloc_i_long_demean <- df_alloc_i_long %>%
      group_by(rho) %>%
      mutate(EH_star_i_mean = mean(EH_star_i)) %>%
      mutate(!!sym(svr_D_star_i_demean) := !!sym(svr_EH_star_i)/EH_star_i_mean)

    # 7a. Demeaned Gini Calculations
    ar_gini_D_star <- df_alloc_i_long %>% select(one_of(svr_rho, svr_D_star_i)) %>%
      group_by(!!sym(svr_rho)) %>%
      do(D_star_gini = ff_dist_gini_vector_pos(.[[svr_D_star_i]])) %>%
      unnest(c(D_star_gini)) %>% pull()
    ar_gini_EH_star <- df_alloc_i_long_demean %>%
      select(one_of(svr_rho, svr_D_star_i_demean)) %>%
      group_by(!!sym(svr_rho)) %>%
        do(EH_star_gini = ff_dist_gini_vector_pos(.[[svr_D_star_i_demean]])) %>%
        unnest(c(EH_star_gini)) %>% pull()
    # 7b. variance
    ar_sd_D_star <- df_alloc_i_long %>% select(one_of(svr_rho, svr_D_star_i)) %>%
      group_by(!!sym(svr_rho)) %>%
      do(D_star_sd = sd(.[[svr_D_star_i]])) %>%
      unnest(c(D_star_sd)) %>% pull()
    ar_sd_EH_star <- df_alloc_i_long %>% select(one_of(svr_rho, svr_EH_star_i)) %>%
      group_by(!!sym(svr_rho)) %>%
        do(EH_star_sd = sd(.[[svr_EH_star_i]])) %>%
        unnest(c(EH_star_sd)) %>% pull()
    # 7c. min and mean outcomes
    ar_mean_EH_star <- df_alloc_i_long %>% select(one_of(svr_rho, svr_EH_star_i)) %>%
      group_by(!!sym(svr_rho)) %>%
        do(EH_star_mean = mean(.[[svr_EH_star_i]])) %>%
        unnest(c(EH_star_mean)) %>% pull()
    ar_min_EH_star <- df_alloc_i_long %>% select(one_of(svr_rho, svr_EH_star_i)) %>%
      group_by(!!sym(svr_rho)) %>%
        do(EH_star_min = min(.[[svr_EH_star_i]])) %>%
        unnest(c(EH_star_min)) %>% pull()
    # 7d. atkinson inequality
    ar_atkinson_EH_star <- df_alloc_i_long_demean %>%
      select(one_of(svr_rho, svr_rho_val, svr_D_star_i_demean)) %>%
      group_by(!!sym(svr_rho)) %>%
      mutate(beta = 1/n()) %>%
      mutate(EH_star_atk_compo = beta*((!!sym(svr_D_star_i_demean))^(!!sym(svr_rho_val)))) %>%
      summarize(EH_star_atk = 1 - sum(EH_star_atk_compo)^(1/(!!sym(svr_rho_val)))) %>%
      pull(EH_star_atk)
    # 7z. collect stats
    mt_rho_gini <- cbind(ar_rho, ar_gini_D_star,
                         ar_gini_EH_star,
                         ar_atkinson_EH_star,
                         ar_sd_D_star, ar_sd_EH_star,
                         ar_mean_EH_star, ar_min_EH_star)
    colnames(mt_rho_gini) <- c(svr_rho_val, 'gini_D_star',
                               'gini_EH_star',
                               'atkinson_EH_star',
                               'sd_D_star', 'sd_EH_star',
                               'mean_EH_star', 'min_EH_star')
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
