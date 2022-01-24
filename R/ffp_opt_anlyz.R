ffp_opt_anlyz_rhgin <- function(df, svr_id_i,
                                svr_A_i, svr_alpha_i, svr_beta_i,
                                fl_N_agg, ar_rho,
                                svr_inpalc = "optiallocate",
                                svr_expout = "opti_exp_outcome") {
  #' Theorem 2, Bounded Linear Allocation solution, loop along a vector of
  #' planner inequality preference (rho)
  #'
  #' @description Works with linear allocation problems. The function invokes
  #' ffp_opt_solin_relow, has parameters that are also required for that
  #' function. The addition here is *fl_rho* becomes *ar_rho*
  #'
  #' @param df tibble data table including variables using svr names below each
  #'   row is potentially an individual who will receive alternative allocations
  #' @param svr_id_i string name of the identify key for each i, to make sure
  #'   merging happens properly
  #' @param svr_A_i string name of the A_i variable, dot product of covariates
  #'   and coefficients
  #' @param svr_alpha_i string name of the alpha_i variable, individual-specific
  #'   (marginal-effects)
  #' @param svr_beta_i string name of the beta_i variable, relative preference
  #'   weight for each child
  #' @param fl_N_agg float total resource avaible for allocation, if not
  #'   specific, sum by svr_N_i
  #' @param ar_rho array preferences for equality for the planner, each value
  #'   from negative infinity to 1
  #' @param svr_inpalc string variable name for newly generated input optimal
  #'   allocation
  #' @param svr_expout string variable name for newly generated expected outcome
  #' @return a dataframe that expands the df inputs with additional results.
  #' @return a list with a dataframe and an array \itemize{ \item
  #'   df_opti_alloc_all_rho - table with columns for optimal allocation and exp
  #'   outcome different rhos \item mt_opti_alloc_all_rho - a matrix row = indi,
  #'   col = rho, value = optimal allocation \item mt_expc_outcm_all_rho - a
  #'   matrix row = indi, col = rho, value = exp outcome optimal allocations
  #'   \item mt_gini - a matrix row = rhos, col = opti-allo and exp-out, value =
  #'   gini }
  #' @author Fan Wang, \url{http://fanwangecon.github.io}
  #' @references
  #' \url{https://fanwangecon.github.io/PrjOptiAlloc/reference/ffp_opt_anlyz_rhgin.html}
  #' \url{https://fanwangecon.github.io/PrjOptiAlloc/articles/ffv_opt_anlyz_rhgin.html}
  #' @export
  #' @import dplyr tidyr stringr broom REconTools
  #' @examples
  #' data(df_opt_dtgch_cbem4)
  #' df <- df_opt_dtgch_cbem4
  #' svr_id_i <- 'indi.id'
  #' svr_A_i <- 'A_lin'
  #' svr_alpha_i <- 'alpha_lin'
  #' svr_beta_i <- 'beta'
  #' fl_N_agg <- 10000
  #' ar_rho = c(-50, -10, -0.1, 0.1, 0.5, 0.7)
  #' ls_lin_solu_all_rhos <- ffp_opt_anlyz_rhgin(df, svr_id_i, svr_A_i, svr_alpha_i, svr_beta_i, fl_N_agg, ar_rho)
  #' df_opti_alloc_all_rho <- ls_lin_solu_all_rhos$df_opti_alloc_all_rho
  #' summary(df_opti_alloc_all_rho)
  #' mt_opti_alloc_all_rho <- ls_lin_solu_all_rhos$mt_opti_alloc_all_rho
  #' summary(mt_opti_alloc_all_rho)
  #' mt_expc_outcm_all_rho <- ls_lin_solu_all_rhos$mt_expc_outcm_all_rho
  #' summary(mt_expc_outcm_all_rho)
  #' mt_gini <- ls_lin_solu_all_rhos$mt_gini
  #' summary(mt_gini)

  df_opti_alloc_all_rho <- df
  it_indi_count <- dim(df)[1]

  # A. First Loop over Planner Preference ----
  # Generate Rank Order
  for (it_rho_ctr in seq(1, length(ar_rho))) {
    fl_rho <- ar_rho[it_rho_ctr]

    # B. Invoke optimal linear (crs) solution problem ----
    # ar_opti is the array of optimal choices, it is in df_opti as well. use
    # df_opti for merging, because that contains the individual keys. actually
    # file here should contain unique keys, unique key ID as required input.
    # should return? actually it is fine, the function here needs the key, not
    # solin_flinr
    ls_lin_solu <- ffp_opt_solin_relow(
      df, svr_A_i, svr_alpha_i, svr_beta_i, fl_N_agg, fl_rho,
      svr_inpalc, svr_expout
    )

    # C. Keep for df collection individual key + optimal allocation ----
    # _on stands for optimal nutritional choices
    # _eh stands for expected height
    tb_opti_allocate_wth_key <- ls_lin_solu$df_opti %>%
      select(one_of(svr_id_i, svr_inpalc, svr_expout)) %>%
      rename(
        !!paste0("rho_c", it_rho_ctr, "_", svr_inpalc) := !!sym(svr_inpalc),
        !!paste0("rho_c", it_rho_ctr, "_", svr_expout) := !!sym(svr_expout)
      )

    # D. merge optimal allocaiton results from different planner preference ----
    df_opti_alloc_all_rho <- df_opti_alloc_all_rho %>%
      left_join(tb_opti_allocate_wth_key, by = svr_id_i)
  }

  # E. Extract from All results Optimal Allocation and Expected Outcomes ----
  mt_opti_alloc_all_rho <- data.matrix(df_opti_alloc_all_rho %>% select(ends_with(svr_inpalc)))
  mt_expc_outcm_all_rho <- data.matrix(df_opti_alloc_all_rho %>% select(ends_with(svr_expout)))

  # F. Compute gini for each rho ----
  # ff_dist_gini_vector_pos() is from REconTools
  ar_opti_alloc_gini <- suppressMessages(apply(t(mt_opti_alloc_all_rho), 1, ff_dist_gini_vector_pos))
  ar_expc_outcm_gini <- suppressMessages(apply(t(mt_expc_outcm_all_rho), 1, ff_dist_gini_vector_pos))

  # G. Wide to Long to Wide Gini ----
  # column names look like: rho_c1_on rho_c2_on rho_c3_on rho_c1_eh rho_c2_eh rho_c3_eh
  tb_gini_onerow_wide <- cbind(as_tibble(t(ar_opti_alloc_gini)), as_tibble(t(ar_expc_outcm_gini)))
  tb_gini_long <- tb_gini_onerow_wide %>%
    pivot_longer(
      cols = starts_with("rho"),
      names_to = c("it_rho_ctr", "oneh"),
      names_pattern = "rho_c(.*)_(.*)",
      values_to = "gini"
    ) %>%
    pivot_wider(
      id_cols = it_rho_ctr,
      names_from = oneh,
      values_from = gini
    )
  planner_elas <- log(1 / (1 - ar_rho) + 2)
  mt_gini <- data.matrix(cbind(tb_gini_long, planner_elas))

  # Returns----
  return(list(
    df_opti_alloc_all_rho = df_opti_alloc_all_rho,
    mt_opti_alloc_all_rho = mt_opti_alloc_all_rho,
    mt_expc_outcm_all_rho = mt_expc_outcm_all_rho,
    mt_gini = mt_gini
  ))
}

ffp_opt_anlyz_rhgin_bin <- function(df, svr_id_i,
                                    svr_A_i = "A", svr_alpha_i = "alpha", svr_beta_i = "beta",
                                    ar_rho = c(0.5, 0.1, -1, -20),
                                    svr_rho = "rho", svr_rho_val = "rho_val",
                                    svr_inpalc = "rank",
                                    svr_expout = "opti_exp_outcome",
                                    verbose = FALSE) {
  #' Theorem 1, Binary Optimal Allocation solution, loop along a vector of
  #' planner inequality preference (rho)
  #'
  #' @description Works with binary allocation problems. The function invokes
  #' ffp_opt_sobin_target_row. The example tests think of as bottled protein.
  #'
  #' @param df tibble data table including variables using svr names below each
  #'   row is potentially an individual who will receive alternative allocations
  #' @param svr_id_i string name of the identify key for each i, to make sure
  #'   merging happens properly
  #' @param svr_A_i string name of the A_i variable, dot product of covariates
  #'   and coefficients
  #' @param svr_alpha_i string name of the alpha_i variable, individual specific
  #'   elasticity information
  #' @param svr_beta_i string name of the beta_i variable, relative preference
  #'   weight for each child
  #' @param ar_rho array preferences for equality for the planner, each value
  #'   from negative infinity to 1
  #' @param svr_rho string variable name for the index planner inequality
  #'   aversion variable, initially rho, then called rho, this is the index
  #'   variable, index for each one of the rho values
  #' @param svr_rho_val string variable name for the value of the planner
  #'   inequality aversion variable
  #' @param svr_inpalc string variable name for newly generated input optimal
  #'   allocation
  #' @param svr_expout string variable name for newly generated expected outcome
  #' @param verbose boolean print out intermediary function outputs
  #' @return a list with a dataframe and an array \itemize{ \item df_all_rho -
  #'   table where optimal targets given rhos are additional columns, note
  #'   rank_max = highest rank reachest, lowest number \item df_all_rho_long -
  #'   long version of df_all_rho, single column all targets another column rho
  #'   values }
  #' @author Fan Wang, \url{http://fanwangecon.github.io}
  #' @references
  #' \url{https://fanwangecon.github.io/PrjOptiAlloc/reference/ffp_opt_anlyz_rhgin_bin.html}
  #' \url{https://fanwangecon.github.io/PrjOptiAlloc/articles/ffv_opt_sobin_rkone_allrw_car.html}
  #' @export
  #' @import dplyr tidyr tibble stringr broom REconTools
  #' @examples
  #' library(dplyr)
  #' data(df_opt_dtgch_cbem4)
  #' df <- df_opt_dtgch_cbem4
  #' svr_id_i <- 'indi.id'
  #' svr_A_i <- 'A_lin'
  #' svr_alpha_i <- 'alpha_lin'
  #' svr_beta_i <- 'beta'
  #' ar_rho = c(-50, -10, -0.1, 0.1, 0.5, 0.7)
  #' ls_bin_solu_all_rhos <- ffp_opt_anlyz_rhgin_bin(df, svr_id_i, svr_A_i, svr_alpha_i, svr_beta_i, ar_rho)
  #' df_all_rho <- ls_bin_solu_all_rhos$df_all_rho
  #' head(df_all_rho %>% select(svr_id_i, svr_A_i, svr_alpha_i, starts_with("rho")), 30)
  #' head(df_all_rho, 15)
  #' summary(df_all_rho)
  #' df_all_rho_long <- ls_bin_solu_all_rhos$df_all_rho_long
  #' head(df_all_rho_long, 15)
  #' summary(df_all_rho_long)

  ar_A <- df %>% pull(!!sym(svr_A_i))
  ar_alpha <- df %>% pull(!!sym(svr_alpha_i))
  ar_beta <- df %>% pull(!!sym(svr_beta_i))

  df_all_rho <- df

  # A. First Loop over Planner Preference
  # Generate Rank Order
  for (it_rho_ctr in seq(1, length(ar_rho))) {
    rho <- ar_rho[it_rho_ctr]

    queue_rank <- ffp_opt_sobin_target_row(df[1, ], rho, ar_A, ar_alpha, ar_beta, svr_A_i, svr_alpha_i, svr_beta_i)$ar_it_rank
    tb_with_rank <- df %>% add_column(queue_rank)

    # m. Keep for df collection individual key + optimal allocation
    # _on stands for optimal nutritional choices
    # _eh stands for expected height
    tb_opti_allocate_wth_key <- tb_with_rank %>%
      select(one_of(svr_id_i, "queue_rank")) %>%
      rename(!!paste0(svr_rho, "_c", it_rho_ctr, "_rk") := !!sym("queue_rank"))

    # n. merge optimal allocaiton results from different planner preference
    df_all_rho <- df_all_rho %>% left_join(tb_opti_allocate_wth_key, by = svr_id_i)
  }

  # o. print results
  if (verbose) {
    summary(df_all_rho)
    str(df_all_rho)
  }

  # Make Longer
  st_bisec_prefix <- paste0(svr_rho, "_c")
  svr_abfafb_long_name <- svr_rho
  svr_bisect_iter <- "nothing"
  svr_number_col <- svr_inpalc
  df_all_rho_long <- df_all_rho %>%
    pivot_longer(
      cols = starts_with(st_bisec_prefix),
      names_to = c(svr_abfafb_long_name, svr_bisect_iter),
      names_pattern = paste0(st_bisec_prefix, "(.*)_(.*)"),
      values_to = svr_number_col
    ) %>%
    mutate(!!sym(svr_rho) := as.numeric(!!sym(svr_rho)))

  # Generate min and max rank for each given the specturm of rho
  # note rank_max = highest rank reachest, lowest number
  df_rank_min_max <- df_all_rho_long %>%
    group_by(!!sym(svr_id_i)) %>%
    summarise(rank_max = min(!!sym(svr_number_col)), rank_min = max(!!sym(svr_number_col)), avg_rank = mean(!!sym(svr_number_col)))

  # Join min and max rank info to wide dataframe
  df_all_rho <- df_all_rho %>% inner_join(df_rank_min_max)

  # add in a column for rho value
  df_all_rho_long <- df_all_rho_long %>%
    left_join(as_tibble(ar_rho) %>%
      rename_all(~ c(svr_rho_val)) %>%
      rowid_to_column(var = svr_rho),
    by = svr_rho
    )

  # Variables Ordering
  df_all_rho <- df_all_rho %>%
    select(!!sym(svr_id_i), !!sym(svr_A_i), !!sym(svr_alpha_i), !!sym(svr_beta_i), rank_min, rank_max, avg_rank, everything())
  df_all_rho_long <- df_all_rho_long %>%
    select(!!sym(svr_id_i), !!sym(svr_A_i), !!sym(svr_alpha_i), !!sym(svr_beta_i), !!sym(svr_rho), !!sym(svr_rho_val), !!sym(svr_number_col))

  # Rank Variations
  # in case cases, if A and alpha are perfectly negatively correlated, there could be common ranking across rho.
  it_how_many_vary_rank <- sum(df_all_rho$rank_max - df_all_rho$rank_min)

  # Returns----
  return(list(
    df_all_rho = df_all_rho,
    df_all_rho_long = df_all_rho_long,
    it_how_many_vary_rank = it_how_many_vary_rank
  ))
}