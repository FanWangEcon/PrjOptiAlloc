## ----GlobalOptions, echo = T, results = 'hide', message=F, warning=F------------------------------------------------------------------------
rm(list = ls(all.names = TRUE))
options(knitr.duplicate.label = 'allow')

## ----loadlib, echo = T, results = 'hide', message=F, warning=F------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(tibble)
library(forcats)
library(ggplot2)

library(REconTools)

library(PrjOptiAlloc)

library(knitr)
library(kableExtra)


## ----opti alloc n2 generate test------------------------------------------------------------------------------------------------------------
ffi_draw_n_alpha <- function(fl_w_dollar = 100,
                             it_w_units_avg = 5,
                             fl_eh_max_inc_i = 5,
                             fl_min_max_eh_inc_ratio = 1.5,
                             fl_disc_cts_match_ratio = 1.1,
                             fl_A_i_alpha_i1_ratio = 2,
                             fl_A_i_relative_ratio = 3,
                             it_N = 2,
                             it_rand_seed = 123){
  #' @param it_w_units_avg float number of goods for cts at max, approxi number for dis
  #' @param fl_eh_max_inc_i1 float maximum increase EH for one individual
  #' @param fl_min_max_eh_inc_ratio float ratio of maximum and minimum outcome increase
  #' @param it_N integer the number of individuals
  #' @param fl_disc_cts_match_ratio float if 1, disc unit = it_w_units_avg
  #' @param fl_A_i_alpha_i1_ratio float A_i divide the alphi_il ratio, discrete set this

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

# Call Function
# ls_out_n4 <- suppressMessages(ffi_draw_n_alpha(it_N = 4))
# if called function without seed first, it_rand_seed fixed
it_rand_seed <- sample(10^7, 1, replace=T)
fl_w_dollar = 100
it_rand_seed <- 4596553
it_N <- 2
ls_out_n2 <- suppressMessages(
  ffi_draw_n_alpha(fl_w_dollar=fl_w_dollar,
                   it_N = it_N, it_rand_seed=it_rand_seed))
ls_out_n2


fl_beta_1 = 0.5
fl_w_dollar = 100
it_w_units_avg = 5
it_rand_seed = sample(10^7, 1, replace=T)
it_rand_seed = 8135788
it_N = 2

ffi_2n_ind_cho_rep_gendata <- function(fl_beta_1 = 0.5,
                                       fl_w_dollar = 100,
                                       it_w_units_avg = 5,
                                       it_rand_seed = 8135788,
                                       it_N = 2) {

suppressMessages(ffi_2n_ind_cho_rep_gendata())

  fl_beta_2 = 1-fl_beta_1

  # Draw data
  ls_out_n2 <- suppressMessages(
    ffi_draw_n_alpha(fl_w_dollar=fl_w_dollar,
                     it_w_units_avg=it_w_units_avg,
                     it_N = it_N, it_rand_seed=it_rand_seed))

  # Translate to 2D budget indiff units
  # fl_e = endowment points
  fl_A_1 = ls_out_n2$ar_A_i[1]
  fl_A_2 = ls_out_n2$ar_A_i[2]
  # fl_alpha_1l = alpha_il
  # fl_eh_alloc_il = outcome at alloations
  ar_alpha_1l = ls_out_n2$ls_ar_alpha_il[[1]]$ar_levels
  ar_alpha_2l = ls_out_n2$ls_ar_alpha_il[[2]]$ar_levels
  ar_alpha_1l_zr = c(0,ls_out_n2$ls_ar_alpha_il[[1]]$ar_levels)
  ar_alpha_2l_zr = c(0,ls_out_n2$ls_ar_alpha_il[[2]]$ar_levels)
  ar_A_1l <- fl_A_1 + cumsum(head(ar_alpha_1l_zr, -1))
  ar_A_2l <- fl_A_2 + cumsum(head(ar_alpha_2l_zr, -1))
  ar_eh_1l_zr <- fl_A_1 + cumsum(ar_alpha_1l_zr)
  ar_eh_2l_zr <- fl_A_2 + cumsum(ar_alpha_2l_zr)
  it_D_1l <- length(ar_alpha_1l)
  it_D_2l <- length(ar_alpha_2l)
  # Max Discrete Choice After which budget outside of choice strike zone fully
  it_w_max <- length(ar_alpha_1l) + length(ar_alpha_2l)
  ar_w_solve_at_disc <- seq(1, it_w_max)

  # Continuous
  fl_alpha_1 = ls_out_n2$ar_cts_alpha[1]
  fl_alpha_2 = ls_out_n2$ar_cts_alpha[2]
  fl_price = ls_out_n2$fl_price
  # continuous choice use discrete max as bounds
  fl_D_max_1 = length(ar_alpha_1l)
  fl_D_max_2 = length(ar_alpha_2l)
  fl_EH_max_1 = fl_A_1 + fl_D_max_1*fl_alpha_1
  fl_EH_max_2 = fl_A_2 + fl_D_max_2*fl_alpha_2
  # max_dollar
  fl_w_dollar_max = fl_price*it_w_max
  ar_w_dollars_strike_at_disc <- seq(0, fl_w_dollar_max, length.out=(it_w_max))
  ar_w_dollars_solve_at_disc <- seq(0, fl_w_dollar_max, length.out=100)

  # print
  message(print(paste0('it_rand_seed:',it_rand_seed)))
  message(print(paste0('fl_A_1:', fl_A_1, ',fl_A_2:', fl_A_2)))
  message(print(paste0('fl_alpha_1:', fl_A_1, ',fl_alpha_2:', fl_A_2)))
  message(print(paste0('ar_alpha_1l:', ar_alpha_1l)))
  message(print(paste0('ar_alpha_2l:', ar_alpha_2l)))
  message(print(paste0('ar_A_1l:', ar_A_1l)))
  message(print(paste0('ar_A_2l:', ar_A_2l)))
  message(print(paste0('ar_eh_1l_zr:', ar_eh_1l_zr)))
  message(print(paste0('ar_eh_2l_zr:', ar_eh_2l_zr)))


  ## -------------------------------------------------------------------------------------------------------------------------------------------
  ls_mt_strike_eh_disc <- vector(mode = "list", length = it_w_units_avg)

  for (it_w in ar_w_solve_at_disc){

    ar_cur_w_ehlt_combo <- c()
    ar_cur_w_unit_combo <- c()

    for (it_1l in seq(1, length(ar_alpha_1l_zr))){
      fl_eh_1l <- ar_eh_1l_zr[it_1l]

      for (it_2l in seq(1, length(ar_alpha_2l_zr))){
        fl_eh_2l <- ar_eh_2l_zr[it_2l]

        # included alloca = 0 at L162
        it_1l_2l_sum <- (it_1l-1) + (it_2l-1)
        if (it_1l_2l_sum == it_w){
          # at this point, EH_1 and EH_2

          # Aggregate Resource is Fully Distributed
          ar_cur_w_ehlt_combo <- c(ar_cur_w_ehlt_combo, fl_eh_1l, fl_eh_2l)
          ar_cur_w_unit_combo <- c(ar_cur_w_unit_combo, it_1l-1, it_2l-1)

        }
      }
    }

    dim(ar_cur_w_ehlt_combo) <- c(2, length(ar_cur_w_ehlt_combo)/2)
    dim(ar_cur_w_unit_combo) <- c(2, length(ar_cur_w_unit_combo)/2)
    mt_strike_eh_disc <- cbind(it_w*fl_price, it_w,
                               t(ar_cur_w_ehlt_combo),
                               t(ar_cur_w_unit_combo))
    colnames(mt_strike_eh_disc) <-
      c('w_dollars', 'w_units', 'EH_1l', 'EH_2l', 'D_1l', 'D_2l')
    ls_mt_strike_eh_disc[[it_w]] <- mt_strike_eh_disc

  }

  tb_strike_eh_disc_all <-
    as_tibble(do.call(rbind, ls_mt_strike_eh_disc)) %>%
    mutate(vartype = 'discrete') %>%
    mutate(outtype = 'strike')

  message(print(tb_strike_eh_disc_all))


  ## -------------------------------------------------------------------------------------------------------------------------------------------
  # assume 1 is the x-axis and 2 is the y-axis
  ffi_cts_strike_crs_slope_intercept <-
    function(fl_w, fl_alpha_price_rat_1, fl_alpha_price_rat_2, fl_A_1, fl_A_2) {

      fl_slope =
        (-1)*(fl_alpha_price_rat_2/fl_alpha_price_rat_1)

      fl_intercept =
        fl_w*fl_alpha_price_rat_2 +
        (fl_alpha_price_rat_2/fl_alpha_price_rat_1)*fl_A_1 +
        fl_A_2

      return(list(fl_slope=fl_slope, fl_intercept=fl_intercept))
    }


  ## -------------------------------------------------------------------------------------------------------------------------------------------
  # Define Marginal Effects and Price Ratios
  fl_alpha_price_rat_1 <- fl_alpha_1/fl_price
  fl_alpha_price_rat_2 <- fl_alpha_2/fl_price

  # Continuous slope generator
  ls_ar_draws_shares = sapply(ar_w_dollars_strike_at_disc,
                              ffi_cts_strike_crs_slope_intercept,
                              fl_alpha_price_rat_1=fl_alpha_price_rat_1,
                              fl_alpha_price_rat_2=fl_alpha_price_rat_2,
                              fl_A_1=fl_A_1,
                              fl_A_2=fl_A_2)
  mt_ar_draws_shares <-
    cbind(ar_w_dollars_strike_at_disc, as_tibble(t(ls_ar_draws_shares)) %>% unnest()) %>%
    rowid_to_column(var = "row_ctr")

  # Print results
  message(print(mt_ar_draws_shares))


  ## -------------------------------------------------------------------------------------------------------------------------------------------
  ls_mt_strike_eh_cts <-
    apply(mt_ar_draws_shares, 1,
          function(row) {
            it_row_ctr <- row[1]
            fl_w_dollars <- row[2]
            fl_slope <- row[3]
            fl_intercept <- row[4]
            ar_EH1 <- seq(fl_A_1, fl_EH_max_1, length.out=100)
            ar_EH2 <- fl_intercept + fl_slope*ar_EH1

            ar_EH1 <- ar_EH1[ar_EH2<=fl_EH_max_2]
            ar_EH2 <- ar_EH2[ar_EH2<=fl_EH_max_2]

            ar_EH1 <- ar_EH1[ar_EH2>=fl_A_2]
            ar_EH2 <- ar_EH2[ar_EH2>=fl_A_2]

            ar_D_1l <- (ar_EH1 - fl_A_1)/fl_alpha_1
            ar_D_2l <- (ar_EH2 - fl_A_2)/fl_alpha_2

            fl_w_units <- (fl_w_dollars/fl_price)
            mt_strike_eh_cts <-
              cbind(fl_w_dollars, fl_w_units,
                    ar_EH1, ar_EH2, ar_D_1l, ar_D_2l)
            rownames(mt_strike_eh_cts) <- NULL

            colnames(mt_strike_eh_cts) <-
              c('w_dollars', 'w_units',
                'EH_1l', 'EH_2l', 'D_1l', 'D_2l')


            return(mt_strike_eh_cts)
          })

  tb_strike_eh_cts_all <-
    as_tibble(do.call(rbind, ls_mt_strike_eh_cts)) %>%
    mutate(vartype = 'continuous') %>%
    mutate(outtype = 'strike')

  message(print(head(tb_strike_eh_cts_all, 10)))


  ## -------------------------------------------------------------------------------------------------------------------------------------------
  fl_dis_w <- (it_w_units_avg + 1)
  fl_cts_w <- fl_dis_w # common price
  ar_rho <- c(0.5, -5)
  ar_rho <- c(0.99, -0.01, -100)
  ar_rho <- c(0.99, -0.01, -6)
  # ar_rho <- 1 - (10^(c(seq(-2,2, length.out=20))))


  ## -------------------------------------------------------------------------------------------------------------------------------------------
  # ID and max Discrete Allocation
  mt_i_D <- cbind(c(1,2), c(it_D_1l, it_D_2l))
  colnames(mt_i_D) <- c('id_i', 'D_max_i')
  tb_i_D <- as_tibble(mt_i_D)
  # A_i and alpha_il as matrix
  mt_A_alpha <- rbind(cbind(1, seq(1,it_D_1l), ar_alpha_1l, ar_A_1l, fl_beta_1),
                      cbind(2, seq(1,it_D_2l), ar_alpha_2l, ar_A_2l, fl_beta_2))
  colnames(mt_A_alpha) <- c('id_i', 'D_il', 'alpha_il', 'A_il', 'beta_i')
  # Combine to generate input_il matrix
  df_input_il <- tb_i_D %>%
    uncount(D_max_i) %>%
    rowid_to_column(var = "id_il") %>%
    left_join(tb_i_D, by='id_i') %>%
    group_by(id_i) %>%
    mutate(D_il = row_number()) %>%
    left_join(as_tibble(mt_A_alpha), by=(c('id_i'='id_i', 'D_il'='D_il')))


  ## -------------------------------------------------------------------------------------------------------------------------------------------
  ls_dis_solu <- suppressWarnings(suppressMessages(
    ffp_opt_anlyz_rhgin_dis(ar_rho,
                            fl_dis_w,
                            df_input_il,
                            bl_df_alloc_il = TRUE,
                            bl_return_V = TRUE,
                            bl_return_allQ_V = TRUE,
                            bl_return_inner_V = TRUE)))
  df_queue_il_long_n2 <-ls_dis_solu$df_queue_il_long
  df_queue_il_wide_n2 <- ls_dis_solu$df_queue_il_wide
  df_alloc_i_long_n2 <- ls_dis_solu$df_alloc_i_long
  df_rho_gini_n2 <- ls_dis_solu$df_rho_gini
  df_alloc_il_long_n2 <- ls_dis_solu$df_alloc_il_long


  ## -------------------------------------------------------------------------------------------------------------------------------------------
  ls_mt_opti_eh_disc <- apply(cbind(ar_w_solve_at_disc), 1,
                              function(row){
                                mt_opti_disc <- suppressWarnings(suppressMessages(
                                  ffp_opt_anlyz_rhgin_dis(
                                    ar_rho,
                                    row[1],
                                    df_input_il,
                                    bl_df_alloc_il = TRUE,
                                    bl_return_V = TRUE,
                                    bl_return_allQ_V = TRUE,
                                    bl_return_inner_V = TRUE)$df_alloc_i_long)) %>%
                                  select(rho_val, D_star_i, EH_star_i) %>%
                                  pivot_wider(names_from = id_i,
                                              values_from = c(D_star_i, EH_star_i)) %>%
                                  rename(EH_1l = EH_star_i_1, EH_2l = EH_star_i_2,
                                         D_1l = D_star_i_1, D_2l = D_star_i_2) %>%
                                  mutate(w_units = row[1], w_dollars = row[1]*fl_price) %>%
                                  select(w_dollars, w_units, EH_1l, EH_2l, D_1l, D_2l, rho_val)
                                return(mt_opti_disc)
                              })
  tb_opti_eh_disc_all <-
    as_tibble(do.call(rbind, ls_mt_opti_eh_disc)) %>%
    mutate(vartype = 'discrete') %>%
    mutate(outtype = 'optimal')
  message(print(head(tb_opti_eh_disc_all, 10)))


  ## -------------------------------------------------------------------------------------------------------------------------------------------
  # ID and max Discrete Allocation
  # Note the problem here does not explicity
  mt_cts <- cbind(c(1,2),
                  c(fl_A_1, fl_A_2),
                  c(fl_alpha_1, fl_alpha_2),
                  c(fl_beta_1, fl_beta_2))
  colnames(mt_cts) <- c('id_i', 'A_i', 'alpha_i', 'beta_i')
  tb_cts <- as_tibble(mt_cts)


  ## -------------------------------------------------------------------------------------------------------------------------------------------
  df <- tb_cts
  svr_id_i <- 'id_i'
  svr_A_i <- 'A_i'
  svr_alpha_i <- 'alpha_i'
  svr_beta_i <- 'beta_i'
  fl_N_agg <- fl_cts_w/fl_price
  ls_lin_solu_all_rhos <- ffp_opt_anlyz_rhgin(df, svr_id_i, svr_A_i, svr_alpha_i, svr_beta_i, fl_N_agg, ar_rho)
  df_opti_alloc_all_rho_n2 <- ls_lin_solu_all_rhos$df_opti_alloc_all_rho
  mt_opti_alloc_all_rho_n2 <- ls_lin_solu_all_rhos$mt_opti_alloc_all_rho
  mt_expc_outcm_all_rho_n2 <- ls_lin_solu_all_rhos$mt_expc_outcm_all_rho
  mt_gini_n2 <- ls_lin_solu_all_rhos$mt_gini


  ## -------------------------------------------------------------------------------------------------------------------------------------------
  ls_mt_opti_eh_cts <- apply(
    cbind(ar_w_dollars_solve_at_disc), 1,
    function(row){

      fl_w_dollars <- row[1]
      fl_w_units <- fl_w_dollars/fl_price

      ls_lin_solu_all_rhos <- suppressWarnings(suppressMessages(
        ffp_opt_anlyz_rhgin(
          df, svr_id_i, svr_A_i, svr_alpha_i, svr_beta_i, fl_w_units, ar_rho)))
      mt_opti_alloc_all_rho_n2 <- ls_lin_solu_all_rhos$mt_opti_alloc_all_rho
      mt_expc_outcm_all_rho_n2 <- ls_lin_solu_all_rhos$mt_expc_outcm_all_rho

      mt_opti_cts <- cbind(fl_w_dollars, fl_w_units,
                           t(mt_expc_outcm_all_rho_n2), t(mt_opti_alloc_all_rho_n2),
                           ar_rho)
      colnames(mt_opti_cts) <- c('w_dollars', 'w_units',
                                 'EH_1l', 'EH_2l', 'D_1l', 'D_2l', 'rho_val')

      return(as_tibble(mt_opti_cts))
    })

  tb_opti_eh_cts_all <-
    as_tibble(do.call(rbind, ls_mt_opti_eh_cts)) %>%
    mutate(vartype = 'continuous') %>%
    mutate(outtype = 'optimal')
  message(print(head(tb_opti_eh_cts_all, 10)))


  ## -------------------------------------------------------------------------------------------------------------------------------------------
  # 1. generate new *D* variable for amount that exceeds bounds.
  tb_opti_eh_cts_all <- tb_opti_eh_cts_all %>%
    mutate(D_1l_excess =
             case_when(D_1l > fl_D_max_1
                       ~ (D_1l - fl_D_max_1),
                       TRUE
                       ~ 0),
           D_2l_excess =
             case_when(D_2l > fl_D_max_2
                       ~ (D_2l - fl_D_max_2),
                       TRUE
                       ~ 0)) %>%
    select(w_dollars, EH_1l, EH_2l, D_1l, D_2l,
           D_1l_excess, D_2l_excess, everything())

  # 2. if both exceed, then reset outcomes for both
  tb_opti_eh_cts_all <- tb_opti_eh_cts_all %>%
    mutate(EH_1l =
             case_when(D_1l_excess > 0 & D_2l_excess > 0
                       ~ EH_1l - D_1l_excess*(fl_alpha_1),
                       TRUE ~ EH_1l),
           EH_2l =
             case_when(D_1l_excess > 0 & D_2l_excess > 0
                       ~ EH_2l - D_2l_excess*(fl_alpha_2),
                       TRUE ~ EH_2l))

  # 3. 1 exceeds but 2 does not
  tb_opti_eh_cts_all <- tb_opti_eh_cts_all %>%
    mutate(EH_1l =
             case_when(D_1l_excess > 0 & D_2l_excess == 0
                       ~ EH_1l - D_1l_excess*(fl_alpha_1),
                       TRUE ~ EH_1l),
           EH_2l =
             case_when(D_1l_excess > 0 & D_2l_excess == 0
                       ~ EH_2l + D_1l_excess*(fl_alpha_2),
                       TRUE ~ EH_2l))
  # %>%
  #   mutate(EH_2l =
  #            case_when(EH_2l >= fl_EH_max_2 ~ fl_EH_max_2,
  #                      TRUE ~ EH_2l))

  # 4. 2 exceeds but 1 does not
  tb_opti_eh_cts_all <- tb_opti_eh_cts_all %>%
    mutate(EH_1l =
             case_when(D_1l_excess == 0 & D_2l_excess > 0
                       ~ EH_1l + D_2l_excess*(fl_alpha_1),
                       TRUE ~ EH_1l),
           EH_2l =
             case_when(D_1l_excess == 0 & D_2l_excess > 0
                       ~ EH_2l - D_2l_excess*(fl_alpha_2),
                       TRUE ~ EH_2l)) %>%
    # mutate(EH_1l =
    #          case_when(EH_1l > fl_EH_max_1 ~ fl_EH_max_1,
    #                    TRUE ~ EH_1l)) %>%
    select(-D_1l_excess, -D_2l_excess)


  ## -------------------------------------------------------------------------------------------------------------------------------------------
  # Combine optimal choices as well as choicesets
  tb_all_points <-
    rbind(tb_strike_eh_disc_all %>% mutate(rho_val = NA),
          tb_opti_eh_disc_all,
          tb_strike_eh_cts_all %>% mutate(rho_val = NA),
          tb_opti_eh_cts_all)

  # Evaluate to obtain Value at Optimal Choices and Utility at Other
  # choice points.
  ffi_util_eval <- function(EH_1l, EH_2l, rho_val) {
    fl_util = (fl_beta_1*(EH_1l^rho_val) + fl_beta_2*(EH_2l^rho_val))^(1/rho_val)
    return(fl_util)
  }
  tb_all_points <- tb_all_points %>%
    mutate(val_at_choices = ffi_util_eval(EH_1l, EH_2l, rho_val))

  # Summary results
  message(message(print(summary(tb_all_points))))

  ## -------------------------------------------------------------------------------------------------------------------------------------------
  # A as x-axis, need bounds on A
  fl_A_min = 0.01
  fl_A_max = it_D_1l+it_D_1l/2
  it_A_grid = 10000

  # Define parameters
  # ar_lambda <- 1 - (10^(c(seq(-2,2, length.out=3))))
  ar_beta <- fl_beta_1

  # Select v_points
  # Indifference at three points, min, max and middle of allocations
  mt_v_star_rho_val <- tb_all_points %>%
    filter(outtype == 'optimal' &
             (w_units == 5 )
           # & (rho_val == ar_rho[2])
    ) %>%
    select(w_units, vartype, rho_val, val_at_choices)

  # Combine for inputs to indiff function
  tb_pref <- as_tibble(mt_v_star_rho_val) %>%
    expand_grid(ar_beta) %>%
    rename_all(~c('w_units','vartype','rho_val', 'vstar', 'beta')) %>%
    rowid_to_column(var = "indiff_id") %>%
    select(indiff_id, rho_val, beta, vstar, w_units, vartype)

  # Generate indifference points with apply and anonymous function
  # note must only select the numeric arrays
  ls_tb_indiff <- apply(
    tb_pref %>%
      select(indiff_id, rho_val, beta, vstar),
    1, function(x){
      indiff_id <- x[1]
      lambda <- x[2]
      beta <- x[3]
      vstar <- x[4]

      ar_fl_A_indiff <- seq(fl_A_min, fl_A_max, length.out=it_A_grid)
      ar_fl_B_indiff <- (((vstar^lambda) -
                            (beta*ar_fl_A_indiff^(lambda)))/(1-beta))^(1/lambda)
      mt_A_B_indiff <- cbind(indiff_id, lambda, beta, vstar,
                             ar_fl_A_indiff, ar_fl_B_indiff)
      colnames(mt_A_B_indiff) <- c('indiff_id', 'rho_val', 'beta_1', 'val_at_choices',
                                   'EH_1l', 'EH_2l')
      tb_A_B_indiff <- as_tibble(mt_A_B_indiff) %>%
        rowid_to_column(var = "A_grid_id") %>%
        filter(EH_2l >= 0 & EH_2l <= max(ar_fl_A_indiff))
      return(tb_A_B_indiff)
    })
  tb_indiff <- do.call(rbind, ls_tb_indiff) %>% drop_na()
  tb_indiff <- tb_indiff %>%
    left_join(tb_pref %>% select(indiff_id, w_units, vartype), by='indiff_id')


  ## -------------------------------------------------------------------------------------------------------------------------------------------
  tb_all_points <- tb_all_points %>%
    mutate(rho_val = case_when(is.na(rho_val) ~ 9999, TRUE ~ rho_val)) %>%
    mutate(vartype = as_factor(vartype),
           outtype = as_factor(outtype),
           rho_val = as_factor(rho_val)) %>%
    mutate(varouttype = fct_cross(vartype, outtype, sep='_'))

  return(list(tb_all_points=tb_all_points, tb_indiff=tb_indiff))
}

ffi_2n_ind_cho_rep_graph <- function(tb_all_points, tb_indiff){
  # ffi_2n_ind_cho_rep: internal two individual indifference, choices, resource expansion path
  ## -------------------------------------------------------------------------------------------------------------------------------------------
  # Labeling
  st_title <- paste0('Expected Outcomes (Continuous + Discrete Choices)')
  st_subtitle <- paste0('Additional discrete inputs have i and l specific non-increasing effects\n',
                        'Continuous choices assumed to have i specific linear effects')
  st_caption <- paste0('Expected Outcome Two People Example')
  st_x_label <- 'Expected Outcome Individual 1'
  st_y_label <- 'Expected Outcome Individual 2'

  # Graphing
  plt_strike <-
    tb_all_points %>% filter(outtype=='strike') %>%
    ggplot(aes(x=EH_1l, y=EH_2l, colour=vartype, shape=vartype)) +
    geom_jitter(size=4, width = 0.01) +
    labs(title = st_title, subtitle = st_subtitle,
         x = st_x_label, y = st_y_label, caption = st_caption) +
    theme_bw()

  # show
  message(print(plt_strike))

  ## -------------------------------------------------------------------------------------------------------------------------------------------
  # Labeling
  st_title <- paste0('Expected Outcomes (Discrete Choices + Optimal)')
  st_subtitle <- paste0('Planner Preference and Varying Discrete Income Expansion Path')
  st_caption <- paste0('Expected Outcome Two People Example')
  st_x_label <- 'Expected Outcome Individual 1'
  st_y_label <- 'Expected Outcome Individual 2'

  # Graphing
  plt_disc <-
    tb_all_points %>%
    filter(vartype=='discrete') %>%
    ggplot(aes(x=EH_1l, y=EH_2l,
               colour=rho_val, shape=rho_val)) +
    geom_jitter(size=7, width = 0) +
    labs(title = st_title, subtitle = st_subtitle,
         x = st_x_label, y = st_y_label, caption = st_caption) +
    theme_bw()

  # show
  message(print(plt_disc))


  ## -------------------------------------------------------------------------------------------------------------------------------------------
  # Labeling
  st_title <- paste0('Expected Outcomes (Continuous Choices + Optimal)')
  st_subtitle <- paste0('Planner Preference and Varying Continuous Income Expansion Path\n',
                        'Assume marginal effects are heterogeneous for i but constant across l')
  st_caption <- paste0('Expected Outcome Two People Example')
  st_x_label <- 'Expected Outcome Individual 1'
  st_y_label <- 'Expected Outcome Individual 2'

  # Graphing
  plt_cts <-
    tb_all_points %>%
    filter(vartype=='continuous') %>%
    ggplot(aes(x=EH_1l, y=EH_2l,
               colour=rho_val, shape=rho_val)) +
    geom_jitter(size=5, width = 0) +
    labs(title = st_title, subtitle = st_subtitle,
         x = st_x_label, y = st_y_label, caption = st_caption) +
    theme_bw()

  # show
  message(print(plt_cts))


  ## -------------------------------------------------------------------------------------------------------------------------------------------
  # Labeling
  st_title <- paste0('Optimal Choices (Continuous + Discrete)')
  st_subtitle <- paste0('Planner Preference and Varying Continuous Income Expansion Path\n',
                        'Assume marginal effects are heterogeneous for i but constant across l\n',
                        'Discrete and Continuous Problems are kind of on the same scale for comparison')
  st_caption <- paste0('Expected Outcome Two People Example')
  st_x_label <- 'Expected Outcome Individual 1'
  st_y_label <- 'Expected Outcome Individual 2'

  # Graphing
  plt_opti <-
    tb_all_points %>%
    filter(outtype=='optimal') %>%
    ggplot(aes(x=EH_1l, y=EH_2l,
               colour=rho_val, shape=vartype)) +
    geom_jitter(size=2, width = 0) +
    labs(title = st_title, subtitle = st_subtitle,
         x = st_x_label, y = st_y_label, caption = st_caption) +
    theme_bw()

  # show
  message(print(plt_opti))


  ## -------------------------------------------------------------------------------------------------------------------------------------------
  # Labeling
  st_title <- paste0('Indifference Curves Aktinson Atkinson Utility (CES)')
  st_subtitle <- paste0('Each Panel Different Beta Weights lambda is inequaliyt aversion')
  st_caption <- paste0('Indifference Curve 2 Individuals.',
                       '')
  st_x_label <- 'Individual 1 EH'
  st_y_label <- 'Individual 2 EH'

  # indiff_id', 'rho_val', 'beta_1', 'val_at_choices',
  #                                'EH_1l', 'EH_2l

  # Graphing
  plt_indiff <-
    tb_indiff %>%
    mutate(rho_val = as_factor(rho_val),
           beta_1 = as_factor(beta_1),
           val_at_choices = as_factor(val_at_choices)) %>%
    ggplot(aes(x=EH_1l, y=EH_2l, colour=rho_val)) +
    facet_wrap(~ vartype) +
    geom_point(size=1) +
    labs(title = st_title, subtitle = st_subtitle,
         x = st_x_label, y = st_y_label, caption = st_caption) +
    theme_bw()

  # Graphing
  plt_indiff_cts <-
    tb_indiff %>% filter(vartype == 'continuous') %>%
    mutate(rho_val = as_factor(rho_val),
           beta_1 = as_factor(beta_1),
           val_at_choices = as_factor(val_at_choices)) %>%
    ggplot(aes(x=EH_1l, y=EH_2l, colour=rho_val)) +
    geom_point(size=1) +
    labs(title = st_title, subtitle = st_subtitle,
         x = st_x_label, y = st_y_label, caption = st_caption) +
    theme_bw()

  # Graphing
  plt_indiff_disc <-
    tb_indiff %>% filter(vartype == 'discrete') %>%
    mutate(rho_val = as_factor(rho_val),
           beta_1 = as_factor(beta_1),
           val_at_choices = as_factor(val_at_choices)) %>%
    ggplot(aes(x=EH_1l, y=EH_2l, colour=rho_val)) +
    geom_point(size=1) +
    labs(title = st_title, subtitle = st_subtitle,
         x = st_x_label, y = st_y_label, caption = st_caption) +
    theme_bw()

  # show
  message(print(plt_indiff))


  ## ---- widh----------------------------------------------------------------------------------------------------------------------------------
  plt_indiff_disc +
    geom_jitter(data = tb_all_points %>%filter(vartype=='discrete'),
                aes(x=EH_1l, y=EH_2l, colour=rho_val, shape=rho_val),
                size=7, width = 0)


  ## ---- widh----------------------------------------------------------------------------------------------------------------------------------
  plt_indiff_cts +
    geom_jitter(data = tb_all_points %>%filter(vartype=='continuous'),
                aes(x=EH_1l, y=EH_2l, colour=rho_val, shape=rho_val),
                size=7, width = 0)

}
