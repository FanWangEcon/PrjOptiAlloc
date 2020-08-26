ffp_snw_process_inputs <-
  function(srt_simu_path = 'C:/Users/fan/Documents/Dropbox (UH-ECON)/PrjNygaardSorensenWang/Output/',
           snm_simu_csv = 'snwx_v_planner_moredense_a100zh266_e2m2.csv',
           df_plan_v_tilde_full = NULL,
           fl_max_phaseout = 238000,
           it_bin_dollar_before_phaseout = 2500,
           fl_percheck_dollar = 100,
           fl_multiple = 58056,
           it_max_checks = 44,
           fl_tax_hh = 128580000,
           it_max_age = 64,
           it_min_age = 18,
           it_age_bins = 2,
           ar_svr_csv = c('age', 'marital', 'kids', 'checks',	'ymin', 'mass', 'survive', 'vtilde', 'ctilde'),
           ar_svr_groups = c('marital', 'kids', 'age_group', 'ymin_group'),
           ar_svr_groups_stats = c('mass', 'survive'),
           svr_checks = 'checks',
           svr_v_value = 'vtilde',
           svr_c_value = 'ctilde',
           svr_mass = 'mass',
           ar_rho = c(1),
           bl_threshold = FALSE,
           it_check_headorspouse = 12,
           it_check_perkids = 5,
           bl_given_firstcheck = FALSE,
           bl_non_inc_adjust = FALSE,
           bl_print = TRUE,
           bl_print_verbose = FALSE) {
    #' Processing Matlab Simulation results for allocation for Nygaard, Sorernsen and Wang (2020)
    #'
    #' @description
    #' Matlab Simulation Process for Allocation
    #'
    #' @author Fan Wang, \url{http://fanwangecon.github.io}
    #' @export
    #'

    ## ----setup, include=FALSE------------------------------------------------------------------------------------------------------------------------------------
    # rm(list = ls())

    # knitr::opts_chunk$set(echo = TRUE)
    # knitr::opts_chunk$set(fig.width=12, fig.height=8)

    # library(tidyverse)
    # library(REconTools)

    # ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    # # Max Phase Out given 1200*2 + 500*4 = 4400
    # fl_max_phaseout = 238000
    # it_bin_dollar_before_phaseout = 2500
    # # Dollar Per Check
    # fl_percheck_dollar = 100
    # # Meaning of Ymin Ymax simulated interval of 1
    # fl_multiple = 58056
    # # Number of Max Checks
    # it_max_checks = 44
    # # Number of Tax Paying Households
    # fl_tax_hh = 128580000
    # # Number of Income Groups to Use: use 25 for 10,000 = 1
    # # Age Conditions
    # # it_max_age = 64
    # # it_min_age = 64
    # it_max_age = 64
    # it_min_age = 18


    # ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    # # File Path
    # srt_simu_path <- 'C:/Users/fan/Documents/Dropbox (UH-ECON)/PrjNygaardSorensenWang/Output/'
    # # File Name
    # # snm_simu_csv <- 'snwx_v_planner_small.csv'
    # # snm_simu_csv <- 'snwx_v_planner_small_dup.csv'
    # # snm_simu_csv <- 'snwx_v_planner_base.csv'
    # # snm_simu_csv <- 'snwx_v_planner_dense.csv'
    # # snm_simu_csv <- 'snwx_v_planner_densemore.csv'
    # # st_file_type <- 'small_dup'
    # # st_file_type <- 'dense'
    # # st_file_type <- 'moredense'
    # # st_file_type <- 'densemore_a55z133'
    # # st_file_type <- 'densemore_a55z266_e0m0'
    # # st_file_type <- 'moredense_a100z266_e0m0'
    # # st_file_type <- 'moredense_a55zh43zs11'
    # # st_file_type <- 'moredense_a75zh101zs5'
    #
    # # 1. e0m2 very dense a and zh test
    # st_file_type <- 'moredense_a100zh266_e1m1'
    #
    # # 2. e1m1 very dense a and zh test, both married and education, no spouse shock
    # st_file_type <- 'moredense_a100zh266_e2m2'
    #
    # # CSV Name
    # snm_simu_csv <- paste0('snwx_v_planner_',st_file_type,'.csv')
    #
    # # Column Names
    # ar_svr_csv <- c('age', 'marital', 'kids', 'checks',	'ymin', 'mass', 'survive', 'vtilde', 'ctilde')
    # # Variables That Identify Individual Types
    # ar_svr_groups <- c('marital', 'kids', 'age_group', 'ymin_group')
    # ar_svr_groups_stats <- c('mass', 'survive')
    # # Number of Checks and Planner Value
    # svr_checks <- 'checks'
    # svr_v_value <- 'vtilde'
    # svr_c_value <- 'ctilde'


    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    # mt_plan_v_tilde <- read.csv(paste0(srt_simu_path, snm_simu_csv), header=FALSE)

    if (is.null(df_plan_v_tilde_full)) {
      df_plan_v_tilde_full <- as_tibble(read.csv(paste0(srt_simu_path, snm_simu_csv), header=FALSE)) %>%
        rename_all(~c(ar_svr_csv))
    }

    if (bl_given_firstcheck) {
      # second round, include possibly allocating max for first round
      df_plan_v_tilde <- df_plan_v_tilde_full %>%
        filter(vtilde != 0) %>%
        filter(checks <= it_max_checks + 44) %>%
        filter(age <= it_max_age) %>%
        filter(age >= it_min_age)
    } else {
      # first round
      df_plan_v_tilde <- df_plan_v_tilde_full %>%
        filter(vtilde != 0) %>%
        filter(checks <= it_max_checks) %>%
        filter(age <= it_max_age) %>%
        filter(age >= it_min_age)
    }


    # df_plan_v_tilde <- as_tibble(read.csv(paste0(srt_simu_path, snm_simu_csv), header=FALSE)) %>%
    #   rename_all(~c(ar_svr_csv)) %>%
    #   filter(vtilde != 0) %>%
    #   filter(checks <= it_max_checks) %>%
    #   filter(age <= it_max_age) %>%
    #   filter(age >= it_min_age)

    # Remove
    # rm(mt_plan_v_tilde)

    # Column 1: Age (in year before COVID)
    # Column 2: Marital status (0 if not married; 1 if married)
    # Column 3: Nr of kids (0, 1, ..., 5) where 5 means 5 or more
    # Column 4: Number of welfare checks (here either equal to 0 or 1)
    # Column 5 and column 6 give income range
    # So the individual's income is at least as large as the value in column 5 but strictly less than the value in column 6
    # Column 7: Population weight Of that particular group (in the stationary distribution)
    # Column 8: Survival probability of that particular age (since the planner knows that some of the individuals will die before next period, so wasn't sure how you wanted me to include that. I did not already include it in V^tilde)
    # Column 9: Value of planner as in the slides (with the exception that I didn't multiply by the survival probability


    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    if (bl_print_verbose){
      REconTools::ff_summ_percentiles(df_plan_v_tilde)
    }

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    if (bl_print){
      print(paste0('sum(df_plan_v_tilde %>% pull(mass)=', sum(df_plan_v_tilde %>% pull(mass))))
    }


    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    # Age Groups
    if (it_max_age < 64) {

      ar_agecut = seq(it_min_age-1, it_max_age, length.out=it_age_bins+1)

    } else {

      if (it_age_bins == 4) {
        # G4 grouping
        if (it_max_age == 64) {
          # four groups
          ar_agecut = c(it_min_age-1, 30, 40, 50, 64)
        } else {
          ar_agecut = c(it_min_age-1, 30, 40, 50, 64, it_max_age)
        }
      } else {
        ar_agecut = seq(it_min_age-1, it_max_age, length.out=it_age_bins+1)
      }

    }

    # Dimensions
    if (bl_print){
      print(paste0('dim(df_plan_v_tilde)=',dim(df_plan_v_tilde)))
    }

    # df_plan_v_tilde_yjm <- df_plan_v_tilde
    df_plan_v_tilde_yjm <- df_plan_v_tilde %>%
      mutate(age_group = (cut(age, ar_agecut))) %>%
      group_by(marital, kids, checks, ymin, age_group) %>%
      summarize(vtilde = sum(vtilde*mass)/sum(mass),
                ctilde = sum(ctilde*mass)/sum(mass),
                mass = sum(mass),
                survive = mean(survive)) %>%
      ungroup()

    # Remove
    rm(df_plan_v_tilde)

    # Summarize
    if (bl_print){
      print(paste0('dim(df_plan_v_tilde_yjm)=',dim(df_plan_v_tilde_yjm)))
    }
    if (bl_print_verbose){
      REconTools::ff_summ_percentiles(df_plan_v_tilde_yjm)
    }

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    # Number of Cuts Before th Mas Phase Out Point.
    fl_thres = fl_max_phaseout/fl_multiple
    # Solution Grid Precision prior to max_phaseout point.
    inc_grid1 = seq(0, fl_thres, length.out=(fl_max_phaseout)/it_bin_dollar_before_phaseout)
    fl_grid_gap = inc_grid1[2] - inc_grid1[1]
    # Not all inc_grid1 points are valid, there are some elements that have no mass
    fl_min_ymin_posmass = min(df_plan_v_tilde_yjm %>% pull(ymin))
    # First group is min
    ar_ycut = c(0, inc_grid1[inc_grid1>fl_min_ymin_posmass+fl_grid_gap], 7)

    # alternative method
    # fl_max_full_phaseout = fl_max_phaseout/fl_multiple
    # ar_ycut2 = c(0, seq(
    #   min(df_plan_v_tilde_yjm %>% pull(ymin)),
    #   fl_max_full_phaseout,
    #   length.out=(it_inc_groups - 1))[2:(it_inc_groups - 1)], 7)

    # ar_ycut = c(0, seq(
    #   min(df_plan_v_tilde_yjm %>% pull(ymin)),
    #   7,
    #   length.out=(it_inc_groups - 1))[2:(it_inc_groups - 1)])

    if (bl_print){
      print(paste0('ar_ycut*fl_multiple:',ar_ycut*fl_multiple))
    }
    it_inc_groups = length(ar_ycut)
    fl_inc_gap = (ar_ycut[3]-ar_ycut[2])*fl_multiple
    fl_inc_min = min(df_plan_v_tilde_yjm %>% pull(ymin))*fl_multiple
    subtitle = paste0('1 unit along x-axis = $', round(fl_inc_gap),
                      ', x-axis min = $', round(fl_inc_min),
                      ', x-axis final group >= $', round(ar_ycut[length(ar_ycut)-1]*fl_multiple))
    if (bl_print){
      print(paste0('subtitle:',subtitle))
    }

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    # df_plan_v_tilde_ygrpjm %>% filter(kids==0 & checks == 1)
    # table(df_plan_v_tilde_ygrpjm$ymin_group)
    # df_plan_v_tilde_ygrpjm <- df_plan_v_tilde_yjm %>% mutate(ymin_group = as.factor(ymin))
    df_plan_v_tilde_ygrpjm <- df_plan_v_tilde_yjm %>%
      mutate(ymin_group = (cut(ymin, ar_ycut))) %>%
      group_by(marital, kids, checks, age_group, ymin_group) %>%
      summarize(vtilde = sum(vtilde*mass)/sum(mass),
                ctilde = sum(ctilde*mass)/sum(mass),
                mass = sum(mass),
                survive = mean(survive)) %>%
      ungroup()

    # Remove
    rm(df_plan_v_tilde_yjm)

    # Summarize
    if (bl_print_verbose){
      REconTools::ff_summ_percentiles(df_plan_v_tilde_ygrpjm)
    }

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    if (bl_print){
      print(paste0('sum(df_plan_v_tilde_ygrpjm %>% pull(mass))',sum(df_plan_v_tilde_ygrpjm %>% pull(mass))))
    }

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    # group id
    svr_group_id <- 'group_id'
    # Define
    ls_svr_group_vars <- ar_svr_groups
    # panel dataframe following
    df_plan_v_tilde_id <- df_plan_v_tilde_ygrpjm %>%
      arrange(!!!syms(ls_svr_group_vars)) %>%
      group_by(!!!syms(ls_svr_group_vars)) %>%
      mutate(!!sym(svr_group_id) := (row_number()==1)*1) %>%
      ungroup() %>%
      rowid_to_column(var = "id") %>%
      mutate(!!sym(svr_group_id) := cumsum(!!sym(svr_group_id))) %>%
      select(one_of(svr_group_id, ls_svr_group_vars), everything())

    if (bl_print_verbose){
      REconTools::ff_summ_percentiles(df_plan_v_tilde_id)
    }

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    if (bl_print_verbose){
      REconTools::ff_summ_count_unique_by_groups(df_plan_v_tilde_id,ls_svr_group_vars,svr_group_id)
    }
    if (bl_print_verbose){
      REconTools::ff_summ_percentiles(df_plan_v_tilde_id, bl_statsasrows = FALSE)
    }

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    # Select Grouping by Variables
    df_id <- df_plan_v_tilde_id %>%
      select(one_of(svr_group_id, ls_svr_group_vars, ar_svr_groups_stats)) %>%
      group_by(!!!syms(svr_group_id)) %>%
      slice_head() %>% ungroup() %>%
      select(one_of(svr_group_id, ls_svr_group_vars, ar_svr_groups_stats)) %>%
      rename(id_i = !!sym(svr_group_id))
    ar_group_ids <- unique(df_id %>% pull(id_i))

    # Summarize
    if (bl_print_verbose){
      REconTools::ff_summ_percentiles(df_id)
    }

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    if (bl_print){
      print(paste0('sum(df_id %>% pull(mass))', sum(df_id %>% pull(mass))))
    }

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    # Select 4 variables
    df_value <- df_plan_v_tilde_id %>%
      select(one_of(svr_group_id, svr_checks, svr_v_value, svr_c_value))
    # remove
    rm(df_plan_v_tilde_id)
    # Summarize
    if (bl_print_verbose){
      REconTools::ff_summ_percentiles(df_value)
      REconTools::ff_summ_count_unique_by_groups(df_value, svr_group_id, svr_group_id)
    }

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    # 1. id column and id_il
    df_il <- df_value %>% rename(id_i = !!sym(svr_group_id)) %>%
      mutate(id_il = row_number()) %>%
      select(id_i, id_il, everything())
    # 2. D_max_i and D_il
    df_il <- df_il %>%
      arrange(id_i, svr_checks) %>% group_by(id_i) %>%
      mutate(D_max_i = max(!!sym(svr_checks))) %>%
      rename(D_il = !!sym(svr_checks)) %>%
      mutate(beta_i = 1/n()) %>%
      select(id_i, id_il, D_max_i, D_il, everything())
    # Summarize
    if (bl_print_verbose){
      REconTools::ff_summ_percentiles(df_il)
    }

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    # 3. A_il and alpha_il
    df_il_U <- df_il %>%
      mutate(c_alpha_il = lead(!!sym(svr_c_value)) - (!!sym(svr_c_value)),
             v_alpha_il = lead(!!sym(svr_v_value)) - (!!sym(svr_v_value))) %>%
      rename(c_A_il = !!sym(svr_c_value),
             v_A_il = !!sym(svr_v_value)) %>%
      ungroup()

    # 4. drop max check
    df_il_U <- df_il_U %>%
      filter(D_il != max(df_il$D_il)) %>%
      mutate(D_il = D_il + 1)

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    # https://fanwangecon.github.io/PrjOptiAlloc/reference/df_opt_caschool_input_il.html
    if (bl_print_verbose){
      # id_i id_il D_max_i  D_il  A_il alpha_il  beta_i
      head(df_il_U, 50)
      tail(df_il_U, 50)
      # Summarize
      REconTools::ff_summ_percentiles(df_il_U)
    }

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    # Rescale
    # df_il_U <- df_il_U %>%
    #   mutate(v_A_il = v_A_il + 30) %>%
    #   mutate(v_A_il = case_when(v_A_il >= 0.01 ~ v_A_il,
    #                             v_A_il <  0.01 ~ 0.01 ))
    # Minimum A
    fl_min_v_A_il <- min(df_il_U$v_A_il) + 0.01
    # Rescale by minimum
    df_il_U <- df_il_U %>%
      mutate(v_A_il = v_A_il - fl_min_v_A_il)

    # Summarize
    if (bl_print){
      REconTools::ff_summ_percentiles(df_il_U)
    }

    ## ---- fig.width=5, fig.height=5------------------------------------------------------------------------------------------------------------------------------
    # subset select
    set.seed(123)
    it_draw <- length(ar_group_ids)
    # it_draw <- 30
    ar_group_rand <- ar_group_ids[sample(length(ar_group_ids), it_draw, replace=FALSE)]
    df_input_il <- df_il_U %>%
      filter(id_i %in% ar_group_rand) %>%
      mutate(id_il = row_number())

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    if (bl_non_inc_adjust){
      # Update c_alpha_il
      df_input_il_noninc <- df_input_il %>%
        arrange((D_il)) %>%
        group_by(id_i) %>%
        do(c_alpha_il_noninc = ffi_alpha_non_increasing_adj_flatten(.$c_alpha_il)) %>%
        unnest(c(c_alpha_il_noninc)) %>%
        group_by(id_i) %>%
        mutate(D_il = row_number()) %>%
        left_join(df_input_il, by=(c('id_i'='id_i', 'D_il'='D_il')))

      # Update v_alpha_il
      df_input_il_noninc <- df_input_il_noninc %>%
        arrange((D_il)) %>%
        group_by(id_i) %>%
        do(v_alpha_il_noninc = ffi_alpha_non_increasing_adj_flatten(.$v_alpha_il)) %>%
        unnest(c(v_alpha_il_noninc)) %>%
        group_by(id_i) %>%
        mutate(D_il = row_number()) %>%
        left_join(df_input_il_noninc, by=(c('id_i'='id_i', 'D_il'='D_il')))

      # replace
      df_input_il_noninc <- df_input_il_noninc %>%
        select(-c_alpha_il, -v_alpha_il) %>%
        rename(c_alpha_il = c_alpha_il_noninc) %>%
        rename(v_alpha_il = v_alpha_il_noninc)
    } else {
      df_input_il_noninc <- df_input_il
    }

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    df_input_il_covid_actual <- df_input_il_noninc %>%
      left_join(df_id, by = "id_i") %>%
      mutate(ymin_group = as.numeric(ymin_group)) %>%
      ungroup() %>% rowwise() %>%
      mutate(actual_checks =
               ffi_vox_checks_ykm(ymin_group, marital, kids,
                                  ar_ycut, fl_multiple, fl_percheck_dollar)) %>%
      ungroup()

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    # Summarize:
    if (bl_print){
      REconTools::ff_summ_percentiles(df_input_il_covid_actual)
    }
    if (bl_print_verbose){
      # Group Summarize:
      ls_svr_group_vars_cact <- c('ymin_group', 'age_group', 'marital', 'kids')
      REconTools::ff_summ_bygroup(
        df_input_il_covid_actual, ls_svr_group_vars_cact, 'actual_checks')$df_table_grp_stats
      # Group Summarize:
      REconTools::ff_summ_bygroup(df_input_il_covid_actual, c('ymin_group'), 'actual_checks')$df_table_grp_stats
      REconTools::ff_summ_bygroup(df_input_il_covid_actual, c('marital'), 'actual_checks')$df_table_grp_stats
      REconTools::ff_summ_bygroup(df_input_il_covid_actual, c('kids'), 'actual_checks')$df_table_grp_stats
    }

    ## ----- Reset Based on First Check amounts, Reset df_input_il_noninc
    if (bl_given_firstcheck) {
      # If allocation is positive, that means: *D_il = actual_checks*,
      # keep those rows, the *A* And *alpha* in those rows are correct. If *actual_checks = 0*, that means we need to use row were *D_il=1*, but replace the alpha value there by zero.

      # 2020 consumption
      df_input_il_covid_actual <- df_input_il_covid_actual %>%
        filter(case_when(actual_checks == 0 ~ D_il <= it_max_checks,
                         TRUE ~ D_il >= (actual_checks + 1) & D_il <= (actual_checks + 1 + it_max_checks - 1))) %>%
        filter() %>% arrange(id_i, D_il) %>% group_by(id_i) %>%
        mutate(D_il = row_number()) %>%
        mutate(D_max_i = it_max_checks) %>% ungroup() %>%
        arrange(id_i, D_il) %>% mutate(id_il = row_number())

      # reset df_input_il_noninc
      df_input_il_noninc <- df_input_il_covid_actual %>%
        select(id_i, id_il, D_max_i, D_il, v_A_il, c_A_il, beta_i, c_alpha_il, v_alpha_il)
      # summarize
      if (bl_print_verbose){
        REconTools::ff_summ_percentiles(df_input_il_covid_actual)
      }
    }

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    mass_sum <- df_input_il_covid_actual %>% summarize(mass_sum = sum(mass))
    fl_cost_max_checks_all <- mass_sum*fl_percheck_dollar*fl_tax_hh
    st_covid_check_cost <- paste0('Cost All Max Checks=$', round(fl_cost_max_checks_all/1000000000,2),
                                  ' bil (assume ',round(fl_tax_hh/1000000,2),' mil tax households)')
    if (bl_print){
      print(st_covid_check_cost)
    }

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    mass_sum_covid_vox_actual <- df_input_il_covid_actual %>%
      filter(actual_checks >= D_il) %>%
      summarize(mass_cumsum = sum(mass))
    fl_cost_actual <- mass_sum_covid_vox_actual*fl_percheck_dollar*fl_tax_hh
    st_covid_check_cost <- paste0('VOX Policy Cost=$', round(fl_cost_actual/1000000000,2),
                                  ' bil (assume ',round(fl_tax_hh/1000000,2),
                                  ' mil tax households, use SNW 2020 simulated P(Kids, Marry, INcome))')
    if (bl_print){
      print(st_covid_check_cost)
    }

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    # 2020 consumption
    df_input_ib_c <- df_input_il_covid_actual %>%
      filter(case_when(actual_checks == 0 ~ D_il == 1, # if check = 0, filter D_il = 1
                       TRUE ~ D_il == actual_checks)) %>%
      rename(A_i_l0 = c_A_il) %>%
      mutate(alpha_o_i = case_when(actual_checks == 0 ~ 0,
                                   TRUE ~ c_alpha_il)) %>%
      select(id_i, A_i_l0, alpha_o_i, beta_i, mass, actual_checks) %>%
      mutate(mass_i = mass, beta_i = 1)

    # value
    df_input_ib_v <- df_input_il_covid_actual %>%
      filter(case_when(actual_checks == 0 ~ D_il == 1, # if check = 0, filter D_il = 1
                       TRUE ~ D_il == actual_checks)) %>%
      rename(A_i_l0 = v_A_il) %>%
      mutate(alpha_o_i = case_when(actual_checks == 0 ~ 0,
                                   TRUE ~ v_alpha_il)) %>%
      select(id_i, A_i_l0, alpha_o_i, beta_i, mass, actual_checks) %>%
      mutate(mass_i = mass, beta_i = 1)

    # summarize
    if (bl_print){
      REconTools::ff_summ_percentiles(df_input_ib_c)
      REconTools::ff_summ_percentiles(df_input_ib_v)
    }

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    # Total checks
    it_total_checks <- df_input_il_covid_actual %>%
      filter(D_il == 1) %>%
      summarize(total_checks = sum(actual_checks))
    it_total_checks <- as.numeric(it_total_checks)
    # this is the measure of checks available given VOX allocation and simulated mass
    # summarize
    if (bl_print){
      print(paste0('mass_sum_covid_vox_actual=',mass_sum_covid_vox_actual))
    }
    # And this point, the number is not important
    fl_dis_w <- it_total_checks
    fl_dis_w_mass <- as.numeric(mass_sum_covid_vox_actual)
    if (bl_print){
      print(paste0('fl_dis_w=',fl_dis_w))
    }

    ## ---- Modify Maximum Allocation Point for Each Individual
    # The modification below means that
    if (bl_threshold) {
      if (bl_print){
        print(paste0('(df_input_il_noninc)=',(df_input_il_noninc)))
      }
      # Threshold frame
      df_input_il_noninc <- df_input_il_noninc %>%
        left_join(df_id, by = "id_i") %>%
        mutate(D_max_i = kids*it_check_perkids + marital*it_check_headorspouse + it_check_headorspouse) %>%
        filter(D_max_i >= D_il) %>%
        select(id_i, v_alpha_il, D_il, c_alpha_il, id_il, D_max_i, v_A_il, c_A_il, beta_i) %>%
        ungroup() %>%
        arrange(id_i, D_il) %>%
        mutate(id_il = row_number())
      # Threshold summarize
      if (bl_print_verbose){
        table(df_input_il_noninc$D_max_i)
      }
      # Reduced Dimension
      if (bl_print){
        print(dim(df_input_il_noninc))
      }
    }

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    # Inputs
    # 30 individuals 25 checks, half the amount is available
    df_input_il_c <- df_input_il_noninc %>%
      rename(A_il = c_A_il) %>%
      rename(alpha_il = c_alpha_il) %>%
      select(-v_A_il, -v_alpha_il)

    # merge with mass, and set beta to 1
    df_input_il_c <- df_input_il_c %>%
      mutate(beta_i = 1) %>%
      left_join(df_id %>% select(id_i, mass), by='id_i') %>%
      rename(mass_i = mass) %>%
      select(id_i, id_il, D_max_i, D_il, A_il, alpha_il, beta_i, mass_i) %>%
      ungroup()

    # Solve with Function
    ls_dis_solu_c <- suppressWarnings(suppressMessages(
      ffp_opt_anlyz_rhgin_dis(ar_rho,
                              fl_dis_w_mass,
                              df_input_il_c,
                              bl_df_alloc_il = FALSE,
                              bl_return_V = TRUE,
                              bl_return_allQ_V = FALSE,
                              bl_return_inner_V = FALSE,
                              svr_measure_i = 'mass_i')))
    df_queue_il_long_c <-ls_dis_solu_c$df_queue_il_long
    df_alloc_i_long_c <- ls_dis_solu_c$df_alloc_i_long
    df_rho_gini_c <- ls_dis_solu_c$df_rho_gini


    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    # Inputs
    # 30 individuals 25 checks, half the amount is available
    df_input_il_v <- df_input_il_noninc %>%
      rename(A_il = v_A_il) %>%
      rename(alpha_il = v_alpha_il) %>%
      select(-c_A_il, -c_alpha_il)
    # merge with mass, and set beta to 1
    df_input_il_v <- df_input_il_v %>%
      mutate(beta_i = 1) %>%
      left_join(df_id %>% select(id_i, mass), by='id_i') %>%
      rename(mass_i = mass) %>%
      select(id_i, id_il, D_max_i, D_il, A_il, alpha_il, beta_i, mass_i) %>%
      ungroup()

    # Solve with Function
    ls_dis_solu_v <- suppressWarnings(suppressMessages(
      ffp_opt_anlyz_rhgin_dis(ar_rho,
                              fl_dis_w_mass,
                              df_input_il_v,
                              bl_df_alloc_il = FALSE,
                              bl_return_V = TRUE,
                              bl_return_allQ_V = FALSE,
                              bl_return_inner_V = FALSE,
                              svr_measure_i = 'mass_i')))
    df_queue_il_long_v <-ls_dis_solu_v$df_queue_il_long
    df_alloc_i_long_v <- ls_dis_solu_v$df_alloc_i_long
    df_rho_gini_v <- ls_dis_solu_v$df_rho_gini


    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    if (bl_print_verbose){
      REconTools::ff_summ_percentiles(df_queue_il_long_c, bl_statsasrows = FALSE)
      REconTools::ff_summ_percentiles(df_alloc_i_long_c, bl_statsasrows = FALSE)
      REconTools::ff_summ_percentiles(df_queue_il_long_v, bl_statsasrows = FALSE)
      REconTools::ff_summ_percentiles(df_alloc_i_long_v, bl_statsasrows = FALSE)
    }

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    tb_rho_rev_c <-
      PrjOptiAlloc::ffp_opt_anlyz_sodis_rev(ar_rho,
                                            fl_dis_w_mass,
                                            df_input_ib = df_input_ib_c,
                                            df_queue_il_long_with_V = df_queue_il_long_c,
                                            svr_measure_i = 'mass_i')

    # this is assuming: bl_return_allQ_V = FALSE
    tb_rho_vstar_c <- df_queue_il_long_c %>%
        filter(!is.na(V_star_Q_il)) %>%
        arrange(rho_val, Q_il) %>%
        group_by(rho_val) %>%
        summarise(V_star_resource = max(V_star_Q_il)) %>%
        ungroup()

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    # Display Results
    if (bl_print){
      print(tb_rho_rev_c)
      print(tb_rho_vstar_c)
    }

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    tb_rho_rev_v <-
      PrjOptiAlloc::ffp_opt_anlyz_sodis_rev(ar_rho,
                                            fl_dis_w_mass,
                                            df_input_ib = df_input_ib_v,
                                            df_queue_il_long_with_V = df_queue_il_long_v,
                                            svr_measure_i = 'mass_i')

    # this is assuming: bl_return_allQ_V = FALSE
    tb_rho_vstar_v <- df_queue_il_long_v %>%
        filter(!is.na(V_star_Q_il)) %>%
        arrange(rho_val, Q_il) %>%
        group_by(rho_val) %>%
        summarise(V_star_resource = max(V_star_Q_il)) %>%
        ungroup()

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    # Display Results
    if (bl_print){
      print(tb_rho_rev_v)
    }

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    # Consider only subsets of ages for graphing
    df_input_il_noninc_covar <- df_input_il_noninc %>%
      left_join(df_id, by = "id_i") %>%
      filter(kids <= 4) %>%
      mutate(ymin_group = as.numeric(ymin_group),
             kids = as.factor(kids),
             marital = as.factor(marital),
             age_group = as.factor(age_group),
             checks = D_il)

    # Summarize
    df_alloc_i_long_covar_v <- df_alloc_i_long_v %>%
      left_join(df_id, by = "id_i") %>%
      filter(kids <= 4)
    df_alloc_i_long_covar_c <- df_alloc_i_long_c %>%
      left_join(df_id, by = "id_i") %>%
      filter(kids <= 4)


    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    stg_source = paste0('SNW 2020 Simulation')
    stg_age = paste0('Age Between ', it_min_age, ' and ', it_max_age, '; Income Groups = ', it_inc_groups)
    stg_caption = paste0(stg_source, '\n', stg_age)


    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    df_alloc_i_long_covar_v <- df_alloc_i_long_covar_v %>%
      left_join(df_input_ib_v %>% select(id_i, actual_checks), by='id_i')
    df_alloc_i_long_covar_c <- df_alloc_i_long_covar_c %>%
      left_join(df_input_ib_c %>% select(id_i, actual_checks), by='id_i')

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    df_alloc_i_long_covar_v %>%
      select(D_star_i, actual_checks, kids, marital, ymin_group) %>%
      arrange(ymin_group, marital, kids)

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    # v table
    df_alloc_i_long_covar_v <- df_alloc_i_long_covar_v %>%
      rename(allocate_check_optimal = D_star_i,
             allocate_check_actual = actual_checks) %>%
      pivot_longer(cols = starts_with('allocate_check_'),
                   names_to = c('allocate_type'),
                   names_pattern = paste0("allocate_check_(.*)"),
                   values_to = "checks")

    # c table
    df_alloc_i_long_covar_c <- df_alloc_i_long_covar_c %>%
      rename(allocate_check_optimal = D_star_i,
             allocate_check_actual = actual_checks) %>%
      pivot_longer(cols = starts_with('allocate_check_'),
                   names_to = c('allocate_type'),
                   names_pattern = paste0("allocate_check_(.*)"),
                   values_to = "checks")

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    if (bl_print_verbose){
      ls_svr_groups <- c('ymin_group', 'age_group', 'marital', 'kids')
      for (svr_group in ls_svr_groups) {

        # Group by variable
        print(paste0('current group = ', svr_group))

        # Summarize
        df <- df_alloc_i_long_covar_c
        vars.group <- c('rho_val', svr_group, 'allocate_type')
        var.numeric <- 'checks'
        str.stats.group <- 'allperc'
        ar.perc <- c(0.10, 0.25, 0.50, 0.75, 0.90)
        ls_summ_by_group <- REconTools::ff_summ_bygroup(
          df, vars.group, var.numeric, str.stats.group, ar.perc)
        if (bl_print_verbose){
          print(ls_summ_by_group$df_table_grp_stats)
        }
      }
    }

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    if (bl_print_verbose){
      ls_svr_groups <- c('ymin_group', 'age_group', 'marital', 'kids')
      for (svr_group in ls_svr_groups) {

        # Group by variable
        print(paste0('current group = ', svr_group))

        # Summarize
        df <- df_alloc_i_long_covar_v
        vars.group <- c('rho_val', svr_group, 'allocate_type')
        var.numeric <- 'checks'
        str.stats.group <- 'allperc'
        ar.perc <- c(0.10, 0.25, 0.50, 0.75, 0.90)
        ls_summ_by_group <- REconTools::ff_summ_bygroup(
          df, vars.group, var.numeric, str.stats.group, ar.perc)
        if (bl_print_verbose){
          print(ls_summ_by_group$df_table_grp_stats)
        }

      }
    }

    return(list(df_input_il_noninc_covar=df_input_il_noninc_covar,
                df_queue_il_long_v=df_queue_il_long_v,
                df_alloc_i_long_covar_c=df_alloc_i_long_covar_c,
                df_alloc_i_long_covar_v=df_alloc_i_long_covar_v,
                stg_subtitle=subtitle,
                stg_caption=stg_caption,
                tb_rho_rev_c=tb_rho_rev_c,
                tb_rho_vstar_c=tb_rho_vstar_c,
                tb_rho_rev_v=tb_rho_rev_v,
                tb_rho_vstar_v=tb_rho_vstar_v))
  }


ffp_snw_process_inputs_core <-
  function(srt_simu_path = 'C:/Users/fan/Documents/Dropbox (UH-ECON)/PrjNygaardSorensenWang/Output/',
           snm_simu_csv = 'snwx_v_planner_moredense_a100zh266_e2m2.csv',
           df_plan_v_tilde_full = df_plan_v_tilde_full,
           fl_max_phaseout = 238000,
           it_bin_dollar_before_phaseout = 2500,
           it_bin_dollar_after_phaseout = 10000,
           fl_percheck_dollar = 100,
           fl_multiple = 58056,
           it_max_checks = 44,
           fl_tax_hh = 128580000,
           it_max_age = 64,
           it_min_age = 18,
           ar_ycut = NULL,
           ar_agecut = c(17,29,39,49,64),
           ar_svr_csv = c('age', 'marital', 'kids', 'checks',	'ymin', 'mass', 'survive', 'vtilde', 'ctilde'),
           ar_svr_groups = c('marital', 'kids', 'age_group', 'ymin_group'),
           ar_svr_groups_stats = c('mass', 'survive'),
           svr_checks = 'checks',
           svr_v_value = 'vtilde',
           svr_c_value = 'ctilde',
           svr_mass = 'mass',
           ar_rho = c(1),
           bl_threshold = FALSE,
           it_check_headorspouse = 12,
           it_check_perkids = 5,
           bl_given_firstcheck = FALSE,
           bl_non_inc_adjust = FALSE,
           bl_print = TRUE,
           bl_print_verbose = FALSE) {
    #' Processing Matlab Simulation results for allocation for Nygaard, Sorernsen and Wang (2020) Core.
    #'
    #' @description
    #' Matlab Simulation Process for Allocation
    #'
    #' @author Fan Wang, \url{http://fanwangecon.github.io}
    #' @export
    #'

    # mt_plan_v_tilde <- read.csv(paste0(srt_simu_path, snm_simu_csv), header=FALSE)
    df_plan_v_tilde <- df_plan_v_tilde_full %>%
      filter(vtilde != 0) %>%
      filter(checks <= it_max_checks) %>%
      filter(age <= it_max_age) %>%
      filter(age >= it_min_age)

    # Age Groups
    # if (it_max_age < 64) {
    #   ar_agecut = seq(it_min_age-1, it_max_age, length.out=it_age_bins+1)
    # } else {
    #   if (it_age_bins == 4) {
    #     # G4 grouping
    #     if (it_max_age == 64) {
    #       # four groups
    #       ar_agecut = c(17, 30, 40, 50, 64)
    #     } else {
    #       ar_agecut = c(17, 30, 40, 50, 64, it_max_age)
    #     }
    #   } else {
    #     ar_agecut = seq(it_min_age-1, it_max_age, length.out=it_age_bins+1)
    #   }
    # }

    # df_plan_v_tilde_yjm <- df_plan_v_tilde
    df_plan_v_tilde_yjm <- df_plan_v_tilde %>%
      mutate(age_group = (cut(age, ar_agecut))) %>%
      group_by(marital, kids, checks, ymin, age_group) %>%
      summarize(vtilde = sum(vtilde*mass)/sum(mass),
                ctilde = sum(ctilde*mass)/sum(mass),
                mass = sum(mass),
                survive = mean(survive)) %>%
      ungroup()

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    # Number of Cuts Before th Mas Phase Out Point.
    fl_thres = fl_max_phaseout/fl_multiple
    # Solution Grid Precision prior to max_phaseout point.
    inc_grid1 = seq(0, fl_thres, length.out=(fl_max_phaseout)/it_bin_dollar_before_phaseout)
    inc_grid2 = seq(fl_thres, 7, length.out=(fl_max_phaseout)/it_bin_dollar_after_phaseout)
    inc_grid1 = unique(c(inc_grid1, inc_grid2))
    fl_grid_gap = inc_grid1[2] - inc_grid1[1]
    # Not all inc_grid1 points are valid, there are some elements that have no mass
    fl_min_ymin_posmass = min(df_plan_v_tilde_yjm %>% pull(ymin))
    # First group is min

    # use externally provided ar_ycut or use ar_ycut calculations here.
    if (is.null (ar_ycut)) {
      ar_ycut = c(0, inc_grid1[inc_grid1>fl_min_ymin_posmass+fl_grid_gap])
    }

    it_inc_groups = length(ar_ycut)
    fl_inc_gap = (ar_ycut[3]-ar_ycut[2])*fl_multiple
    fl_inc_min = min(df_plan_v_tilde_yjm %>% pull(ymin))*fl_multiple
    subtitle = paste0('1 unit along x-axis = $', round(fl_inc_gap),
                      ', x-axis min = $', round(fl_inc_min),
                      ', x-axis final group >= $', round(ar_ycut[length(ar_ycut)-1]*fl_multiple))

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    df_plan_v_tilde_ygrpjm <- df_plan_v_tilde_yjm %>%
      mutate(ymin_group = (cut(ymin, ar_ycut))) %>%
      group_by(marital, kids, checks, age_group, ymin_group) %>%
      summarize(vtilde = sum(vtilde*mass)/sum(mass),
                ctilde = sum(ctilde*mass)/sum(mass),
                mass = sum(mass),
                survive = mean(survive)) %>%
      ungroup()

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    # group id
    svr_group_id <- 'group_id'
    # Define
    ls_svr_group_vars <- ar_svr_groups
    # panel dataframe following
    df_plan_v_tilde_id <- df_plan_v_tilde_ygrpjm %>%
      arrange(!!!syms(ls_svr_group_vars)) %>%
      group_by(!!!syms(ls_svr_group_vars)) %>%
      mutate(!!sym(svr_group_id) := (row_number()==1)*1) %>%
      ungroup() %>%
      rowid_to_column(var = "id") %>%
      mutate(!!sym(svr_group_id) := cumsum(!!sym(svr_group_id))) %>%
      select(one_of(svr_group_id, ls_svr_group_vars), everything())

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    # Select Grouping by Variables
    df_id <- df_plan_v_tilde_id %>%
      select(one_of(svr_group_id, ls_svr_group_vars, ar_svr_groups_stats)) %>%
      group_by(!!!syms(svr_group_id)) %>%
      slice_head() %>% ungroup() %>%
      select(one_of(svr_group_id, ls_svr_group_vars, ar_svr_groups_stats)) %>%
      rename(id_i = !!sym(svr_group_id))
    ar_group_ids <- unique(df_id %>% pull(id_i))

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    # Select 4 variables
    df_value <- df_plan_v_tilde_id %>%
      select(one_of(svr_group_id, svr_checks, svr_v_value, svr_c_value))

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    # 1. id column and id_il
    df_il <- df_value %>% rename(id_i = !!sym(svr_group_id)) %>%
      mutate(id_il = row_number()) %>%
      select(id_i, id_il, everything())
    # 2. D_max_i and D_il
    df_il <- df_il %>%
      arrange(id_i, svr_checks) %>% group_by(id_i) %>%
      mutate(D_max_i = max(!!sym(svr_checks))) %>%
      rename(D_il = !!sym(svr_checks)) %>%
      mutate(beta_i = 1/n()) %>%
      select(id_i, id_il, D_max_i, D_il, everything())

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    # 3. A_il and alpha_il
    df_il_U <- df_il %>%
      mutate(c_alpha_il = lead(!!sym(svr_c_value)) - (!!sym(svr_c_value)),
             v_alpha_il = lead(!!sym(svr_v_value)) - (!!sym(svr_v_value))) %>%
      rename(c_A_il = !!sym(svr_c_value),
             v_A_il = !!sym(svr_v_value)) %>%
      ungroup()

    # 4. drop max check
    df_il_U <- df_il_U %>%
      filter(D_il != max(df_il$D_il)) %>%
      mutate(D_il = D_il + 1)

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    # Rescale
    # df_il_U <- df_il_U %>%
    #   mutate(v_A_il = v_A_il + 30) %>%
    #   mutate(v_A_il = case_when(v_A_il >= 0.01 ~ v_A_il,
    #                             v_A_il <  0.01 ~ 0.01 ))
    # Minimum A
    fl_min_v_A_il <- min(df_il_U$v_A_il) + 0.01
    # Rescale by minimum
    df_il_U <- df_il_U %>%
      mutate(v_A_il = v_A_il - fl_min_v_A_il)

    ## ---- fig.width=5, fig.height=5------------------------------------------------------------------------------------------------------------------------------
    # subset select
    set.seed(123)
    it_draw <- length(ar_group_ids)
    # it_draw <- 30
    ar_group_rand <- ar_group_ids[sample(length(ar_group_ids), it_draw, replace=FALSE)]
    df_input_il <- df_il_U %>%
      filter(id_i %in% ar_group_rand) %>%
      mutate(id_il = row_number())

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    if (bl_non_inc_adjust){
      # Update c_alpha_il
      df_input_il_noninc <- df_input_il %>%
        arrange((D_il)) %>%
        group_by(id_i) %>%
        do(c_alpha_il_noninc = ffi_alpha_non_increasing_adj_flatten(.$c_alpha_il)) %>%
        unnest(c(c_alpha_il_noninc)) %>%
        group_by(id_i) %>%
        mutate(D_il = row_number()) %>%
        left_join(df_input_il, by=(c('id_i'='id_i', 'D_il'='D_il')))

      # Update v_alpha_il
      df_input_il_noninc <- df_input_il_noninc %>%
        arrange((D_il)) %>%
        group_by(id_i) %>%
        do(v_alpha_il_noninc = ffi_alpha_non_increasing_adj_flatten(.$v_alpha_il)) %>%
        unnest(c(v_alpha_il_noninc)) %>%
        group_by(id_i) %>%
        mutate(D_il = row_number()) %>%
        left_join(df_input_il_noninc, by=(c('id_i'='id_i', 'D_il'='D_il')))

      # replace
      df_input_il_noninc <- df_input_il_noninc %>%
        select(-c_alpha_il, -v_alpha_il) %>%
        rename(c_alpha_il = c_alpha_il_noninc) %>%
        rename(v_alpha_il = v_alpha_il_noninc)
    } else {
      df_input_il_noninc <- df_input_il
    }

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    df_input_il_covid_actual <- df_input_il_noninc %>%
      left_join(df_id, by = "id_i") %>%
      mutate(ymin_group = as.numeric(ymin_group)) %>%
      ungroup() %>% rowwise() %>%
      mutate(actual_checks =
               ffi_vox_checks_ykm(ymin_group, marital, kids,
                                  ar_ycut, fl_multiple, fl_percheck_dollar)) %>%
      ungroup()

    ## ----- Reset Based on First Check amounts, Reset df_input_il_noninc
    if (bl_given_firstcheck) {
      # If allocation is positive, that means: *D_il = actual_checks*,
      # keep those rows, the *A* And *alpha* in those rows are correct. If *actual_checks = 0*, that means we need to use row were *D_il=1*, but replace the alpha value there by zero.

      # 2020 consumption
      df_input_il_covid_actual <- df_input_il_covid_actual %>%
        filter(case_when(actual_checks == 0 ~ D_il <= it_max_checks,
                         TRUE ~ D_il >= (actual_checks + 1) & D_il <= (actual_checks + 1 + it_max_checks - 1))) %>%
        filter() %>% arrange(id_i, D_il) %>% group_by(id_i) %>%
        mutate(D_il = row_number()) %>%
        mutate(D_max_i = it_max_checks) %>% ungroup() %>%
        arrange(id_i, D_il) %>% mutate(id_il = row_number())

      # reset df_input_il_noninc
      df_input_il_noninc <- df_input_il_covid_actual %>%
        select(id_i, id_il, D_max_i, D_il, v_A_il, c_A_il, beta_i, c_alpha_il, v_alpha_il)
      # summarize
      if (bl_print_verbose){
        REconTools::ff_summ_percentiles(df_input_il_covid_actual)
      }
    }

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    mass_sum <- df_input_il_covid_actual %>% summarize(mass_sum = sum(mass))
    fl_cost_max_checks_all <- mass_sum*fl_percheck_dollar*fl_tax_hh
    st_covid_check_cost <- paste0('Cost All Max Checks=$', round(fl_cost_max_checks_all/1000000000,2),
                                  ' bil (assume ',round(fl_tax_hh/1000000,2),' mil tax households)')
    if (bl_print){
      print(st_covid_check_cost)
    }

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    mass_sum_covid_vox_actual <- df_input_il_covid_actual %>%
      filter(actual_checks >= D_il) %>%
      summarize(mass_cumsum = sum(mass))
    fl_cost_actual <- mass_sum_covid_vox_actual*fl_percheck_dollar*fl_tax_hh
    st_covid_check_cost <- paste0('VOX Policy Cost=$', round(fl_cost_actual/1000000000,2),
                                  ' bil (assume ',round(fl_tax_hh/1000000,2),
                                  ' mil tax households, use SNW 2020 simulated P(Kids, Marry, INcome))')
    if (bl_print){
      print(st_covid_check_cost)
    }

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    # 2020 consumption
    df_input_ib_c <- df_input_il_covid_actual %>%
      filter(case_when(actual_checks == 0 ~ D_il == 1, # if check = 0, filter D_il = 1
                       TRUE ~ D_il == actual_checks)) %>%
      rename(A_i_l0 = c_A_il) %>%
      mutate(alpha_o_i = case_when(actual_checks == 0 ~ 0,
                                   TRUE ~ c_alpha_il)) %>%
      select(id_i, A_i_l0, alpha_o_i, beta_i, mass, actual_checks) %>%
      mutate(mass_i = mass, beta_i = 1)

    # value
    df_input_ib_v <- df_input_il_covid_actual %>%
      filter(case_when(actual_checks == 0 ~ D_il == 1, # if check = 0, filter D_il = 1
                       TRUE ~ D_il == actual_checks)) %>%
      rename(A_i_l0 = v_A_il) %>%
      mutate(alpha_o_i = case_when(actual_checks == 0 ~ 0,
                                   TRUE ~ v_alpha_il)) %>%
      select(id_i, A_i_l0, alpha_o_i, beta_i, mass, actual_checks) %>%
      mutate(mass_i = mass, beta_i = 1)

    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
    # Total checks
    it_total_checks <- df_input_il_covid_actual %>%
      filter(D_il == 1) %>%
      summarize(total_checks = sum(actual_checks))
    it_total_checks <- as.numeric(it_total_checks)

    # And this point, the number is not important
    fl_dis_w <- it_total_checks
    fl_dis_w_mass <- as.numeric(mass_sum_covid_vox_actual)

    ## ---- Modify Maximum Allocation Point for Each Individual
    # The modification below means that
    if (bl_threshold) {
      # Threshold frame
      df_input_il_noninc <- df_input_il_noninc %>%
        left_join(df_id, by = "id_i") %>%
        mutate(D_max_i = kids*it_check_perkids + marital*it_check_headorspouse + it_check_headorspouse) %>%
        filter(D_max_i >= D_il) %>%
        select(id_i, v_alpha_il, D_il, c_alpha_il, id_il, D_max_i, v_A_il, c_A_il, beta_i) %>%
        ungroup() %>%
        arrange(id_i, D_il) %>%
        mutate(id_il = row_number())
    }

    return(list(mass_sum = mass_sum ,
                mass_sum_covid_vox_actual = mass_sum_covid_vox_actual,
                df_input_il_noninc = df_input_il_noninc,
                df_id = df_id))
  }

ffi_alpha_non_increasing_adj_flatten <- function(ar_alpha, fl_min_inc_bd = 1e-20) {
  # Flattening, flatten so that the marginal value of the next check must be
  # lower than the marginal value of the check prior

  ar_cur <- ar_alpha
  ar_cur_adj <- rep(NA, length(ar_cur))
  ar_cur_adj[1] <- ar_cur[1]

  for (it_ctr in 2:length(ar_cur)) {

    fl_cur_val <- ar_cur[it_ctr]
    fl_last_val <- ar_cur_adj[it_ctr-1]

    if (fl_cur_val > fl_last_val) {
      ar_cur_adj[it_ctr] = fl_last_val
    } else {
      ar_cur_adj[it_ctr] = fl_cur_val
    }

  }

  # return
  return(ar_cur_adj)

}

ffi_alpha_non_increasing_adj_cumusum <- function(ar_alpha, fl_min_inc_bd = 1e-20) {
  # alpha has tiny upticks sometimes due to approximation errors, need to be adjusted
  # Following theorem 1, alpha must be non-increasing.
  # This adjustment, when there are X number of checks, must be made for all Checks
  # at once. This corrects starting from the highest check count.
  # https://www.evernote.com/l/AAq8vDtH5v1B_4Al4Kidhd9Ni1DfLL2PRkc/

  ar_cur <- ar_alpha
  ar_cur_diff <- diff(ar_cur)
  # New Array of NAs
  ar_cur_df_adj <- rep(NA, length(ar_cur_diff))
  # No changes needed if difference is negative, or zero, non-increasing
  ar_cur_df_adj[ar_cur_diff<=0] = 0
  # Record changes if positive
  ar_cur_df_adj[ar_cur_diff>0] = ar_cur_diff[ar_cur_diff>0]
  # Cumulative sum adjustment needed
  ar_cur_adj_cumsum <- cumsum(ar_cur_df_adj)
  ar_cur_adj_cumsum <- c(0, ar_cur_adj_cumsum);
  # Add adjustment to original array
  ar_cur_adj <- ar_cur - ar_cur_adj_cumsum
  # Make sure Adjusted changes are non-negative
  ar_cur_adj[ar_cur_adj < fl_min_inc_bd] = fl_min_inc_bd

  # return
  return(ar_cur_adj)
}



ffi_vox_checks_ykm <- function(ymin_group, marital, kids,
                               ar_ycut,
                               fl_multiple = 58056, fl_percheck_dollar = 100,
                               it_inc_subgroups = 10) {
  # if policy change, also go modify section *Modify Maximum Allocation Point for Each Individual* in main above

  # # Receive at Most
  # fl_max_checks <- 1200*2+4*500
  # # Max Phase Out Group
  # fl_mas_start_drop <- 150000
  # # Drop rate
  # fl_drop_rate <- 5/100
  # # max Phase
  # fl_phase_out_max <- fl_mas_start_drop + fl_max_checks/fl_drop_rate
  # print(fl_phase_out_max)

  # # Poorest married 4 kids
  # ffi_vox_checks_ykm(1,1,4, ar_ycut, fl_multiple, fl_percheck_dollar)
  # # Test Function
  # ffi_vox_checks_ykm(2,0,0, ar_ycut, fl_multiple, fl_percheck_dollar)
  # ffi_vox_checks_ykm(2,1,1, ar_ycut, fl_multiple, fl_percheck_dollar)
  # ffi_vox_checks_ykm(2,1,3, ar_ycut, fl_multiple, fl_percheck_dollar)
  # ffi_vox_checks_ykm(2,1,3, ar_ycut, fl_multiple, fl_percheck_dollar)
  # # Test Function
  # ffi_vox_checks_ykm(4,0,0, ar_ycut, fl_multiple, fl_percheck_dollar)
  # ffi_vox_checks_ykm(4,1,1, ar_ycut, fl_multiple, fl_percheck_dollar)
  # ffi_vox_checks_ykm(4,1,3, ar_ycut, fl_multiple, fl_percheck_dollar)
  # ffi_vox_checks_ykm(4,1,3, ar_ycut, fl_multiple, fl_percheck_dollar)
  # # Test Function
  # ffi_vox_checks_ykm(11,0,0, ar_ycut, fl_multiple, fl_percheck_dollar)
  # ffi_vox_checks_ykm(11,1,1, ar_ycut, fl_multiple, fl_percheck_dollar)
  # ffi_vox_checks_ykm(11,1,3, ar_ycut, fl_multiple, fl_percheck_dollar)
  # ffi_vox_checks_ykm(11,1,3, ar_ycut, fl_multiple, fl_percheck_dollar)

  # marital 0 or 1
  # kids 0,1,2,3
  # ymin_group index from 1 to N

  # income minimum and maximum,
  # by construction: length(ar_ycut_dollar) = max(ymin_group) + 1
  ar_ycut_dollar = ar_ycut*fl_multiple
  it_ygroups = length(ar_ycut_dollar)

  # Default no checks
  fl_check_dollor = 0

  # Only provide checks if not in the last income group, where all receive none
  if (ymin_group < it_ygroups - 1) {

    # start point household head
    fl_check_dollar = 1200

    # married household gets more, marital = 0, 1
    fl_check_dollar = fl_check_dollar + marital*1200

    # Households with kids: 0, 1,2,3,4
    fl_check_dollar = fl_check_dollar + kids*500


    # lower and upper bounds on income
    fl_inc_group_lower_bound = ar_ycut_dollar[ymin_group]
    fl_inc_group_upper_bound = ar_ycut_dollar[ymin_group+1]

    # A grid of income between these two points: 10 points
    ar_inc_grid = seq(fl_inc_group_lower_bound, fl_inc_group_upper_bound,
                      length.out=it_inc_subgroups)

    # What is the tax rate at each point of these incomes given marry and kids?
    ar_check_reduce_inc_grid = matrix(data=NA, nrow=length(ar_inc_grid), ncol=1)

    # as income increases, fl_check_dollar go down
    it_ctr = 0
    for (fl_inc in ar_inc_grid) {

      it_ctr = it_ctr + 1

      if (marital == 0 && kids == 0) {
        # The benefit would start decreasing at a rate of $5 for every additional $100 in income
        fl_check_reduce = ((max(fl_inc - 75000,0))/100)*5
      }

      # phaseout starts $112,500 for heads of household
      if (marital == 0 && kids != 0) {
        # The benefit would start decreasing at a rate of $5 for every additional $100 in income
        fl_check_reduce = ((max(fl_inc - 112500,0))/100)*5
      }

      # phaseout starts $150,000 for heads of household
      if (marital == 1 ) {
        # The benefit would start decreasing at a rate of $5 for every additional $100 in income
        fl_check_reduce = ((max(fl_inc - 150000,0))/100)*5
      }

      ar_check_reduce_inc_grid[it_ctr] = max(0, fl_check_dollar - fl_check_reduce)

    }

    fl_check_dollor = mean(ar_check_reduce_inc_grid)

  }

  # Check Numbers
  fl_avg_checks = round(fl_check_dollor/fl_percheck_dollar, 0)

  return(fl_avg_checks)
}
