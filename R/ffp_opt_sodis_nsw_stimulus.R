ffp_nsw_opt_anlyz_rhgin_dis <- function(ar_rho,
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
                                        svr_A_il = 'A_il', svr_alpha_il = 'alpha_il',
                                        svr_beta_i = 'beta_i', svr_betameasure_i = 'vmassbeta_i',
                                        svr_measure_i = 'mass_i', svr_mass_cumu_il = 'mass_cumu_il',
                                        svr_expout = 'opti_exp_outcome',
                                        svr_V_star_Q_il = 'V_star_Q_il',
                                        st_idcol_prefix = 'sid_'){
  #' Discrete Optimal Allocations, Queue, and Values. NSW version.
  #'
  #' @description Optimal ALlocation Queues for Discrete Problems, allocation
  #'   Amount, Value. This version of the function is adpated specially for
  #'   \href{https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3839890}{Nygaard,
  #'   Sorensen, and Wang (2021)} for the optimal allocation of stimulus checks,
  #'   and is based on \code{\link{ffp_opt_anlyz_rhgin_dis()}}.
  #'
  #' @param fl_teacher_increase_number is the amount of resources (in measure if
  #'   svr_measure_i is not NA) available for allocation.
  #' @param bl_df_alloc_il boolean if true this will output a matrix where each
  #'   column is a different i individual and each row is a position along the
  #'   queue, and each cell is the level of allocation for individual i when the
  #'   overall allocation queue is up to the current queue position.
  #' @param svr_beta_i string variable name for planner bias, used for
  #'   allocation problem, generating the queue.
  #' @param svr_measure_i string variable name for mass for this type of
  #'   recipient, default NA mass of recipient is the measure of recipient of
  #'   this type in the population. This measure does not impact relative
  #'   ranking optimal allocation across types, but determines how much to push
  #'   individual types further along the allocation queue back. used for mass
  #'   at each queue point
  #' @param svr_betameasure_i string variable name for the weight considerating
  #'   both mass and beta jointly, can not simply multiply beta and measure
  #'   together. Suppose two people, beta is 0.5 for each person. And then we
  #'   have 0.5 mass for each person, simply multiplying the two together
  #'   genreates 0.5x0.5 vs 0.5x0.5, don't sum up to the existing total mass
  #'   even. This is used for welfare function
  #' @author Fan Wang, \url{http://fanwangecon.github.io}
  #' @references
  #' \url{https://fanwangecon.github.io/PrjOptiAlloc/reference/ffp_opt_anlyz_rhgin_dis.html}
  #' \url{https://fanwangecon.github.io/PrjOptiAlloc/articles/ffv_opt_sodis_rkone_casch_allrw.html}
  #' @export
  #'

  # Step 1
  # Call function to Solve for Optimal Targeting Queue
  # df_input_il already has vmassbeta_i
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
  if (is.na(svr_measure_i)) {
    # Allocations that would be below resource threshold, discrete units
    # fl_teacher_increase_number = discrete units of available
    df_queue_il_long <- df_queue_il_long %>%
      mutate(!!sym(svr_D_Wbin_il) :=
               case_when(!!sym(svr_inpalc) <= fl_teacher_increase_number ~ 1,
                         TRUE ~ 0))
  } else {
    # Resource Threshold is in Measure, Cumulative Sum measure first
    # fl_teacher_increase_number = measure of available
    # 1. group by rho: svr_rho
    # 2. sort by rank queue: svr_inpalc
    # 3. cumulative sum the measure within rho by queue rank: mass_cumu_queue
    # 4. set cutoff allocating threshold based on measure of resources available: fl_teacher_increase_number
    df_queue_il_long <- df_queue_il_long %>%
      left_join(df_input_il %>%
                  select(!!sym(svr_id_il), !!sym(svr_measure_i)),
                by=svr_id_il) %>%
      group_by(!!sym(svr_rho)) %>%
      arrange(!!sym(svr_inpalc)) %>%
      mutate(!!sym(svr_mass_cumu_il) := cumsum(!!sym(svr_measure_i))) %>%
      ungroup() %>%
      mutate(!!sym(svr_D_Wbin_il) :=
               case_when(!!sym(svr_mass_cumu_il) <= fl_teacher_increase_number ~ 1,
                         TRUE ~ 0))
  }

  # add some variables to long frame
  df_queue_il_long <- df_queue_il_long %>%
    left_join(df_input_il %>%
                select(!!sym(svr_id_i), !!sym(svr_id_il),
                       !!sym(svr_D_max_i), !!sym(svr_D_il)),
              by=svr_id_il)

  if (!is.na(svr_betameasure_i)) {
    df_queue_il_long <- df_queue_il_long %>%
      left_join(df_input_il %>%
                  select(!!sym(svr_id_il), !!sym(svr_betameasure_i)),
                by=svr_id_il)
  }

  # Merge in actual check and income
  # actual checks: to compute results given actual check levels.
  # income: to compute results for all individuals, or individuals below some thresholds.
  df_queue_il_long <- df_queue_il_long %>%
    left_join(df_input_il %>% select(id_i, D_il, actual_checks, ymin_group_str),
              by=(c('id_i'='id_i', 'D_il'='D_il'))) %>%
    mutate(ymin_group_str = as.character(ymin_group_str)) %>%
    ungroup() %>% rowwise() %>%
    mutate(y_group_min = substring(strsplit(ymin_group_str, ",")[[1]][1], 2),
           y_group_max = gsub(strsplit(ymin_group_str, ",")[[1]][2],  pattern = "]", replacement = "")) %>%
    mutate(y_group_min = as.numeric(y_group_min)*58056,
           y_group_max = as.numeric(y_group_max)*58056)

  # Step 3
  # Optimal Allocation Discrete Levels
  svr_select_list <- c(svr_rho, svr_rho_val, svr_id_i, svr_D_max_i, svr_D_Wbin_il, svr_beta_i, 'y_group_max')
  if (!is.na(svr_measure_i)) {
    svr_select_list <- c(svr_select_list, svr_mass_cumu_il)
  }
  if (!is.na(svr_betameasure_i)) {
    svr_select_list <- c(svr_select_list, svr_betameasure_i)
  }


  df_alloc_i_long <- df_queue_il_long %>%
    select(one_of(svr_select_list)) %>%
    group_by(!!sym(svr_id_i), !!sym(svr_rho))

  if (!is.na(svr_measure_i)) {
    df_alloc_i_long <- df_alloc_i_long %>%
      summarize(!!sym(svr_rho_val)  := mean(!!sym(svr_rho_val)),
                !!sym(svr_D_max_i)  := mean(!!sym(svr_D_max_i)),
                !!sym(svr_D_star_i) := sum(!!sym(svr_D_Wbin_il)),
                !!sym(svr_F_star_i) := (!!sym(svr_D_star_i)/!!sym(svr_D_max_i)),
                !!sym(svr_beta_i)  := mean(!!sym(svr_beta_i)),
                !!sym(svr_betameasure_i) := mean(!!sym(svr_betameasure_i)),
                y_group_max = mean(y_group_max)) %>%
      arrange(!!sym(svr_id_i), !!sym(svr_rho))
    # rescale mass weights so mass sum up to 1, for allocation don't need to sum
    # to 1, need to use actual mass, for for gini, and for atkinson and other calcuaiton
    # need to sum up to 1.
    df_alloc_i_long <- df_alloc_i_long %>%
      ungroup() %>%
      mutate(betamsr_i_sum = sum(!!sym(svr_betameasure_i))) %>%
      mutate(betameasure_i_adj = !!sym(svr_betameasure_i)/betamsr_i_sum) %>%
      mutate(!!sym(svr_betameasure_i) := betameasure_i_adj) %>%
      select(-betamsr_i_sum, -betameasure_i_adj)

  } else {
    df_alloc_i_long <- df_alloc_i_long %>%
      summarize(!!sym(svr_rho_val)  := mean(!!sym(svr_rho_val)),
                !!sym(svr_D_max_i)  := mean(!!sym(svr_D_max_i)),
                !!sym(svr_D_star_i) := sum(!!sym(svr_D_Wbin_il)),
                !!sym(svr_F_star_i) := (!!sym(svr_D_star_i)/!!sym(svr_D_max_i)),
                !!sym(svr_beta_i)  := mean(!!sym(svr_beta_i)),
                y_group_max = mean(y_group_max)) %>%
      arrange(!!sym(svr_id_i), !!sym(svr_rho)) %>%
      ungroup()
  }

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

  it_bound_ctr <- 0
  for (it_income_bounds in c(1, 2, 3)) {

    it_bound_ctr <- it_bound_ctr + 1

    bl_compute_gini <- TRUE
    bl_compute_sd <- TRUE
    bl_compute_mean <- TRUE
    bl_compute_min <- TRUE
    bl_compute_atkinson <- TRUE

    if (it_income_bounds == 1) {
      df_queue_il_long_use <- df_queue_il_long
      df_alloc_i_long_use <- df_alloc_i_long
      fl_income_bound <- 1000000
    } else if (it_income_bounds == 2) {
      fl_income_bound <- 200000
      df_queue_il_long_use <- df_queue_il_long %>% filter(y_group_max <= fl_income_bound)
      df_alloc_i_long_use <- df_alloc_i_long %>% filter(y_group_max <= fl_income_bound)
    } else if (it_income_bounds == 3) {
      fl_income_bound <- 160000
      df_queue_il_long_use <- df_queue_il_long %>% filter(y_group_max <= fl_income_bound)
      df_alloc_i_long_use <- df_alloc_i_long %>% filter(y_group_max <= fl_income_bound)
    }

    # Step 5
    # Generate a df that only keeps the A_i from without any allocations, so that we can compute
    # relevant GINI stats given outcomes without allocations. WEIGHTED MEAN
    svr_D0_i_demean <- 'EH_i_demean'
    svr_EH_d0_i <- 'EH_d0_i'
    df_alloc_i_d0 <- df_queue_il_long_use %>% filter(!!sym(svr_D_il) == 1) %>%
      select(one_of(svr_rho, svr_rho_val, svr_id_i, svr_A_il, svr_betameasure_i)) %>%
      mutate(!!sym(svr_EH_d0_i) := !!sym(svr_A_il)) %>%
      ungroup() %>%
      mutate(betamsr_i_sum = sum(!!sym(svr_betameasure_i))) %>%
      mutate(betameasure_i_adj = !!sym(svr_betameasure_i)/betamsr_i_sum) %>%
      mutate(!!sym(svr_betameasure_i) := betameasure_i_adj) %>%
      select(-betamsr_i_sum, -betameasure_i_adj) %>%
      group_by(rho) %>%
      mutate(EH_d0_i_mean = sum(!!sym(svr_betameasure_i)*!!sym(svr_A_il))) %>%
      mutate(!!sym(svr_D0_i_demean) := !!sym(svr_EH_d0_i)/EH_d0_i_mean)

    svr_Dact_i_demean <- 'EH_actual_i_demean'
    svr_EH_dact_i <- 'EH_dact_i'
    df_alloc_i_dact <- df_queue_il_long_use %>%
      filter(case_when(actual_checks == 0 ~ !!sym(svr_D_il) == 1, # if check = 0, filter D_il = 1
                       TRUE ~ !!sym(svr_D_il) == actual_checks)) %>%
      select(one_of(svr_rho, svr_rho_val, svr_id_i, svr_A_il, svr_betameasure_i)) %>%
      mutate(!!sym(svr_EH_dact_i) := !!sym(svr_A_il)) %>%
      ungroup() %>%
      mutate(betamsr_i_sum = sum(!!sym(svr_betameasure_i))) %>%
      mutate(betameasure_i_adj = !!sym(svr_betameasure_i)/betamsr_i_sum) %>%
      mutate(!!sym(svr_betameasure_i) := betameasure_i_adj) %>%
      select(-betamsr_i_sum, -betameasure_i_adj) %>%
      group_by(rho) %>%
      mutate(EH_dact_i_mean = sum(!!sym(svr_betameasure_i)*!!sym(svr_A_il))) %>%
      mutate(!!sym(svr_Dact_i_demean) := !!sym(svr_EH_dact_i)/EH_dact_i_mean)

    # Step 7 Aggregate Statistics: EH_star_mean = EH_star_i_mean. weighted mean
    svr_D_star_i_demean <- 'EH_star_i_demean'
    df_alloc_i_long_demean <- df_alloc_i_long_use %>%
      group_by(rho) %>%
      mutate(EH_star_i_mean = sum(!!sym(svr_betameasure_i)*EH_star_i)) %>%
      mutate(!!sym(svr_D_star_i_demean) := !!sym(svr_EH_star_i)/EH_star_i_mean)

    # Part GINI -------------------
    # 7a1, Gini of choices
    if (bl_compute_gini) {
      ar_gini_drv_D_star <- df_alloc_i_long_use %>%
        select(one_of(svr_rho, svr_D_star_i, svr_betameasure_i)) %>%
        group_by(!!sym(svr_rho)) %>%
        do(D_star_gini_drv = ff_dist_gini_random_var(
          ar_x=.[[svr_D_star_i]],
          ar_prob_of_x = .[[svr_betameasure_i]])) %>%
        unnest(c(D_star_gini_drv)) %>% pull()
      # 7a2. GINI of outcomes
      ar_gini_drv_EH_star <- df_alloc_i_long_demean %>%
        select(one_of(svr_rho, svr_EH_star_i, svr_betameasure_i)) %>%
        group_by(!!sym(svr_rho)) %>%
        do(EH_star_gini_drv = ff_dist_gini_random_var(
          ar_x=.[[svr_EH_star_i]],
          ar_prob_of_x = .[[svr_betameasure_i]])) %>%
        unnest(c(EH_star_gini_drv)) %>% pull()
      # 7a3, zero allocation gini
      ar_gini_drv_EH_d0 <- df_alloc_i_d0 %>%
        select(one_of(svr_rho, svr_EH_d0_i, svr_betameasure_i)) %>%
        group_by(!!sym(svr_rho)) %>%
        do(EH_D0_gini_drv = ff_dist_gini_random_var(
          ar_x=.[[svr_EH_d0_i]],
          ar_prob_of_x = .[[svr_betameasure_i]])) %>%
        unnest(c(EH_D0_gini_drv)) %>% pull()
      # 7a4, actual allocation gini
      ar_gini_drv_EH_dact <- df_alloc_i_dact %>%
        select(one_of(svr_rho, svr_EH_dact_i, svr_betameasure_i)) %>%
        group_by(!!sym(svr_rho)) %>%
        do(EH_Dact_gini_drv = ff_dist_gini_random_var(
          ar_x=.[[svr_EH_dact_i]],
          ar_prob_of_x = .[[svr_betameasure_i]])) %>%
        unnest(c(EH_Dact_gini_drv)) %>% pull()
    }

    # Part Var, and var of log outcome -------------------
    if (bl_compute_sd) {

      # 7b1. variance
      ar_sd_D_star <- df_alloc_i_long_use %>% select(one_of(svr_rho, svr_D_star_i)) %>%
        group_by(!!sym(svr_rho)) %>%
        do(D_star_sd = sd(.[[svr_D_star_i]])) %>%
        unnest(c(D_star_sd)) %>% pull()
      ar_sd_EH_star <- df_alloc_i_long_use %>% select(one_of(svr_rho, svr_EH_star_i)) %>%
        group_by(!!sym(svr_rho)) %>%
        do(EH_star_sd = sd(.[[svr_EH_star_i]])) %>%
        unnest(c(EH_star_sd)) %>% pull()

      # 7b2a, log of expected outcomes
      ar_sd_log_EH_drv_star <- df_alloc_i_long_use %>% select(one_of(svr_rho, svr_EH_star_i, svr_betameasure_i)) %>%
        group_by(!!sym(svr_rho)) %>%
        mutate(log_EH_star = log(!!sym(svr_EH_star_i))) %>%
        mutate(log_EH_star_mean = sum(!!sym(svr_betameasure_i)*log_EH_star)) %>%
        mutate(var_log_EH_star = sum(!!sym(svr_betameasure_i)*(log_EH_star-log_EH_star_mean)^2)) %>%
        summarize(sd_log_EH_star = sqrt(var_log_EH_star)) %>%
        group_by(!!sym(svr_rho)) %>% summarize(sd_log_EH_star = mean(sd_log_EH_star)) %>%
        pull(sd_log_EH_star)
      # 7b2b, log of expected outcomes zero
      ar_sd_log_EH_drv_d0 <- df_alloc_i_d0 %>% select(one_of(svr_rho, svr_EH_d0_i, svr_betameasure_i)) %>%
        group_by(!!sym(svr_rho)) %>%
        mutate(log_EH_d0 = log(!!sym(svr_EH_d0_i))) %>%
        mutate(log_EH_d0_mean = sum(!!sym(svr_betameasure_i)*log_EH_d0)) %>%
        mutate(var_log_EH_d0 = sum(!!sym(svr_betameasure_i)*(log_EH_d0-log_EH_d0_mean)^2)) %>%
        summarize(sd_log_EH_d0 = sqrt(var_log_EH_d0)) %>%
        group_by(!!sym(svr_rho)) %>% summarize(sd_log_EH_d0 = mean(sd_log_EH_d0)) %>%
        pull(sd_log_EH_d0)
      # 7b2c, log of expected outcomes actual
      ar_sd_log_EH_drv_dact <- df_alloc_i_dact %>% select(one_of(svr_rho, svr_EH_dact_i, svr_betameasure_i)) %>%
        group_by(!!sym(svr_rho)) %>%
        mutate(log_EH_dact = log(!!sym(svr_EH_dact_i))) %>%
        mutate(log_EH_dact_mean = sum(!!sym(svr_betameasure_i)*log_EH_dact)) %>%
        mutate(var_log_EH_dact = sum(!!sym(svr_betameasure_i)*(log_EH_dact-log_EH_dact_mean)^2)) %>%
        summarize(sd_log_EH_dact = sqrt(var_log_EH_dact)) %>%
        group_by(!!sym(svr_rho)) %>% summarize(sd_log_EH_dact = mean(sd_log_EH_dact)) %>%
        pull(sd_log_EH_dact)
    }

    # Part Mean ----------------
    if (bl_compute_mean) {
      # 7c.mean outcomes
      ar_mean_EH_star <- df_alloc_i_long_use %>% select(one_of(svr_rho, svr_EH_star_i)) %>%
        group_by(!!sym(svr_rho)) %>%
        do(EH_star_mean = mean(.[[svr_EH_star_i]])) %>%
        unnest(c(EH_star_mean)) %>% pull()

      # mean weighted
      ar_mean_EH_drv_star <- df_alloc_i_long_use %>% select(one_of(svr_rho, svr_EH_star_i, svr_betameasure_i)) %>%
        group_by(!!sym(svr_rho)) %>%
        summarize(EH_star_drv_mean = sum(!!sym(svr_betameasure_i)*!!sym(svr_EH_star_i))) %>%
        pull(EH_star_drv_mean)
      # 7c2a. mean outcome without allocations
      ar_mean_EH_drv_d0 <- df_alloc_i_d0 %>%
        select(one_of(svr_rho, svr_EH_d0_i, svr_betameasure_i)) %>%
        ungroup() %>%
        group_by(!!sym(svr_rho)) %>%
        summarize(EH_d0_drv_mean = sum(!!sym(svr_EH_d0_i)*!!sym(svr_betameasure_i))) %>%
        pull(EH_d0_drv_mean)
      # 7c2b. mean outcome with actual allocations
      ar_mean_EH_drv_dact <- df_alloc_i_dact %>%
        select(one_of(svr_rho, svr_EH_dact_i, svr_betameasure_i)) %>%
        ungroup() %>%
        group_by(!!sym(svr_rho)) %>%
        summarize(EH_dact_drv_mean = sum(!!sym(svr_EH_dact_i)*!!sym(svr_betameasure_i))) %>%
        pull(EH_dact_drv_mean)
    }

    # Part Min ----------------
    if (bl_compute_min) {
      # 7c3. minimum outcome optimal
      ar_min_EH_star <- df_alloc_i_long_use %>% select(one_of(svr_rho, svr_EH_star_i)) %>%
        group_by(!!sym(svr_rho)) %>%
        do(EH_star_min = min(.[[svr_EH_star_i]])) %>%
        unnest(c(EH_star_min)) %>% pull()
      # 7c3a. minimum outcome zero
      ar_min_EH_d0 <- df_alloc_i_d0 %>% select(one_of(svr_rho), EH_d0_i) %>%
        group_by(!!sym(svr_rho)) %>%
        summarize(EH_d0_min = min(EH_d0_i)) %>%
        unnest(c(EH_d0_min)) %>% pull()
      # 7c3b. minimum outcome actual
      ar_min_EH_dact <- df_alloc_i_dact %>% select(one_of(svr_rho), EH_dact_i) %>%
        group_by(!!sym(svr_rho)) %>%
        summarize(EH_dact_min = min(EH_dact_i)) %>%
        unnest(c(EH_dact_min)) %>% pull()
    }

    # Part Atkinson -------------------
    if (bl_compute_atkinson) {
      # 7d1. atkinson inequality
      ar_atkinson_drv_EH_star <- df_alloc_i_long_demean %>%
        select(one_of(svr_rho, svr_rho_val, svr_D_star_i_demean, svr_betameasure_i)) %>%
        group_by(!!sym(svr_rho_val)) %>%
        mutate(EH_star_atk_compo = !!sym(svr_betameasure_i)*((!!sym(svr_D_star_i_demean))^(!!sym(svr_rho_val)))) %>%
        summarize(EH_star_atk_drv = 1 - sum(EH_star_atk_compo)^(1/(!!sym(svr_rho_val)))) %>%
        group_by(!!sym(svr_rho_val)) %>% summarize(EH_star_atk_drv = mean(EH_star_atk_drv)) %>%
        pull(EH_star_atk_drv)
      # 7d2. atkinson inequality with zero allocation
      ar_atkinson_drv_EH_d0 <- df_alloc_i_d0 %>%
        select(one_of(svr_rho, svr_rho_val, svr_D0_i_demean, svr_betameasure_i)) %>%
        group_by(!!sym(svr_rho_val)) %>%
        mutate(EH_d0_atk_compo = !!sym(svr_betameasure_i)*((!!sym(svr_D0_i_demean))^(!!sym(svr_rho_val)))) %>%
        summarize(EH_d0_atk_drv = 1 - sum(EH_d0_atk_compo)^(1/(!!sym(svr_rho_val)))) %>%
        group_by(!!sym(svr_rho_val)) %>% summarize(EH_d0_atk_drv = mean(EH_d0_atk_drv)) %>%
        pull(EH_d0_atk_drv)
      # 7d3. atkinson inequality with actual allocation
      ar_atkinson_drv_EH_dact <- df_alloc_i_dact %>%
        select(one_of(svr_rho, svr_rho_val, svr_Dact_i_demean, svr_betameasure_i)) %>%
        group_by(!!sym(svr_rho_val)) %>%
        mutate(EH_dact_atk_compo = !!sym(svr_betameasure_i)*((!!sym(svr_Dact_i_demean))^(!!sym(svr_rho_val)))) %>%
        summarize(EH_dact_atk_drv = 1 - sum(EH_dact_atk_compo)^(1/(!!sym(svr_rho_val)))) %>%
        group_by(!!sym(svr_rho_val)) %>% summarize(EH_dact_atk_drv = mean(EH_dact_atk_drv)) %>%
        pull(EH_dact_atk_drv)
    }

    # # Step 8
    # # Generate a df that only keeps the A_i from without any allocations, so that we can compute
    # # relevant GINI stats given outcomes without allocations.
    # df_queue_i_d0 <- df_queue_il_long %>% filter(!!sym(svr_D_il) == 1) %>%
    #   select(one_of(svr_id_i, svr_A_il, svr_betameasure_i))
    #
    # df_alloc_i_long_demean <- df_alloc_i_long %>%
    #   group_by(rho) %>%
    #   mutate(EH_star_i_mean = mean(EH_star_i)) %>%
    #   mutate(!!sym(svr_D_star_i_demean) := !!sym(svr_EH_star_i)/EH_star_i_mean)


    # 7z. collect stats
    mt_rho_gini <- cbind(ar_rho,
                         ar_gini_drv_EH_d0, ar_gini_drv_EH_dact, ar_gini_drv_EH_star,
                         ar_sd_log_EH_drv_d0, ar_sd_log_EH_drv_dact, ar_sd_log_EH_drv_star,
                         ar_atkinson_drv_EH_d0, ar_atkinson_drv_EH_dact, ar_atkinson_drv_EH_star,
                         ar_mean_EH_drv_d0, ar_mean_EH_drv_dact, ar_mean_EH_drv_star,
                         ar_min_EH_d0, ar_min_EH_dact, ar_min_EH_star,
                         ar_gini_drv_D_star,
                         ar_sd_D_star,
                         ar_mean_EH_star, ar_sd_EH_star)
    colnames(mt_rho_gini) <- c(svr_rho_val,
                               'gini_drv_EH_d0', 'gini_drv_EH_dact', 'gini_drv_EH_star',
                               'sd_log_EH_drv_d0','sd_log_EH_drv_dact','sd_log_EH_drv_star',
                               'atkinson_drv_EH_d0', 'atkinson_drv_EH_dact', 'atkinson_drv_EH_star',
                               'mean_EH_drv_d0', 'mean_EH_drv_dact', 'mean_drv_EH_star',
                               'min_EH_d0', 'min_EH_dact', 'min_EH_star',
                               'gini_drv_D_star',
                               'sd_D_star',
                               'mean_EH_star', 'sd_EH_star')

    df_rho_gini_cur <- as_tibble(mt_rho_gini) %>% rowid_to_column(var = svr_rho) %>%
      mutate(income_bound = fl_income_bound)

    # Append
    if (it_bound_ctr == 1) {
      df_rho_gini <- df_rho_gini_cur
    } else {
      df_rho_gini <- rbind(df_rho_gini, df_rho_gini_cur)
    }
  }



  # Step 8, value at Q points
  svr_return_list <- c(svr_rho, svr_rho_val, svr_id_i, svr_id_il,
                       svr_D_max_i, svr_D_il, svr_inpalc, svr_D_Wbin_il)
  if (!is.na(svr_measure_i)) {
    svr_return_list <- c(svr_return_list, svr_mass_cumu_il)
  }

  # svr_A_il, svr_alpha_il, svr_beta_i
  if (bl_return_V){
    mt_util_rev_loop <- df_queue_il_long %>%
      group_by(!!sym(svr_rho)) %>%
      do(rev = ffp_nsw_opt_sodis_value(fl_rho = .[[svr_rho_val]],
                                       df_queue_il = .,
                                       bl_return_allQ_V = bl_return_allQ_V,
                                       bl_return_inner_V = bl_return_inner_V,
                                       svr_id_i = svr_id_i, svr_id_il = svr_id_il,
                                       svr_D_il = svr_D_il, svr_inpalc = svr_inpalc, svr_D_Wbin_il = svr_D_Wbin_il,
                                       svr_A_il = svr_A_il, svr_alpha_il = svr_alpha_il,
                                       svr_beta_i = svr_beta_i, svr_betameasure_i = svr_betameasure_i,
                                       svr_V_star_Q_il = svr_V_star_Q_il)) %>%
      unnest()
    # Step 5b, merge values to queue df
    df_queue_il_long <- df_queue_il_long %>% left_join(mt_util_rev_loop %>%
                                                         select(one_of(svr_rho, svr_id_il), starts_with('V_')),
                                                       by=setNames(c(svr_rho, svr_id_il), c(svr_rho, svr_id_il))) %>%
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
      group_by(!!sym(svr_rho))

    df_alloc_il_long <- df_alloc_il_long %>%
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
