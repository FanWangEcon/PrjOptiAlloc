ffp_opt_sobin_target_row <- function(ls_row, fl_rho,
                                     ar_A, ar_alpha, ar_beta,
                                     svr_A_i = "A", svr_alpha_i = "alpha", svr_beta_i = "beta") {
  #' Theorem 1 and 2, solves the binary targeting queue with Ai_i, alpha_i, and
  #' planner preferences, for one individual
  #'
  #' @description This does not solve for the full solution, just the targeting
  #'   queue, individual position on queue.
  #'
  #' @param ls_row list a row from dataframe tibble row where there are
  #'   variables for A, alpha and beta
  #' @param ar_A float array of expected outcome without provision for all N
  #' @param ar_alpha float array of expected effect of provision for each i of N
  #' @param ar_beta float array of planner bias
  #' @param svr_A_i string name of the A_i variable, any nonlinear or linear
  #'   evaluated expected outcome
  #' @param svr_alpha_i string name of the alpha_i variable, individual specific
  #'   effects of treatment
  #' @param svr_beta_i string name of the beta_i variable, relative preference
  #'   weight for each child
  #' @param fl_rho float preference for equality for the planner, negative
  #'   infinity to 1
  #' @return a list with an integer and an array \itemize{ \item ar_rank_val -
  #'   array of relative values based on which rank is computed, relative value
  #'   order same for all rows \item ar_rank - array of index that correspond to
  #'   ar_rank_val \item it_rank - an integer equal to the person's rank on the
  #'   optimal binary targeting queue, smaller number means higher ranking, 1 is
  #'   ranked first to receive allocations }
  #' @author Fan Wang, \url{http://fanwangecon.github.io}
  #' @references
  #' \url{https://fanwangecon.github.io/PrjOptiAlloc/reference/ffp_opt_sobin_target_row.html}
  #' \url{https://fanwangecon.github.io/PrjOptiAlloc/articles/ffv_opt_sobin_rkone_allrw_car.html}
  #' @export
  #' @examples
  #' library(tibble)
  #' library(dplyr)
  #' library(tidyr)
  #' ar_alpha <- c(0.1,1.5,2.5,4)
  #' ar_A <-     c(0.5,1.5,3.5,6.5)
  #' ar_beta <- c(0.25,0.25,0.25,0.25)
  #' mt_alpha_A <- cbind(ar_alpha, ar_A, ar_beta)
  #' ar_st_varnames <- c('alpha', 'A', 'beta')
  #' tb_alpha_A <- as_tibble(mt_alpha_A) %>% rename_all(~c(ar_st_varnames))
  #' tb_alpha_A
  #'
  #' ar_rho <- c(-100, -1.1, 0.01, 0.10, 0.9)
  #' for (it_rho_ctr in seq(1,length(ar_rho))) {
  #'   fl_rho <- ar_rho[it_rho_ctr]
  #'   ls_ranks <- ffp_opt_sobin_target_row(tb_alpha_A[1,], 0.1, ar_A, ar_alpha, ar_beta)
  #'   it_rank <- ls_ranks$it_rank
  #'   ar_it_rank <- ls_ranks$ar_it_rank
  #'   ar_fl_rank_val <- ls_ranks$ar_fl_rank_val
  #'   cat('fl_rho:', fl_rho, 'it_rank:', it_rank, '\n')
  #'   cat('ar_it_rank:', ar_it_rank, '\n')
  #'   cat('ar_fl_rank_val:', ar_fl_rank_val, '\n')
  #' }
  #'

  fl_alpha <- ls_row[[svr_alpha_i]]
  fl_A <- ls_row[[svr_A_i]]
  fl_beta <- ls_row[[svr_beta_i]]

  ar_left <- (((ar_A + ar_alpha)^fl_rho - (ar_A)^fl_rho)
  /
    ((fl_A + fl_alpha)^fl_rho - (fl_A)^fl_rho))
  ar_right <- ((ar_beta) / (fl_beta))
  ar_fl_rank_val <- ar_left * ar_right

  # Not int he form of ranks, but in the form of values.
  # Not needed to compute value/rank row by row, one row is sufficient.
  ar_bl_rank_val <- (ar_fl_rank_val >= 1)
  ar_it_rank <- rank((-1) * ar_fl_rank_val, ties.method = "min")
  it_rank <- sum(ar_bl_rank_val)

  # message(paste0('it_rank:', it_rank))
  # message(ar_full)
  return(list(
    it_rank = it_rank,
    ar_it_rank = ar_it_rank,
    ar_fl_rank_val = ar_fl_rank_val
  ))
}

ffp_opt_sobin_rev <- function(ar_queue_optimal, ar_bin_observed,
                              ar_A, ar_alpha, ar_beta, fl_rho) {
  #' Theorem 1, Equation 6, Resource Equivalent Variation
  #'
  #' @description This does not solve for the full solution, just the targeting
  #'   queue, individual position on queue.
  #'
  #' @param ar_queue_optimal optimal targeting array index of ranking from 1 to
  #'   N targetting queue
  #' @param ar_bin_observed observed vector of binary zeros and ones
  #' @param ar_A float array of expected effect of provision for each i of N
  #' @param ar_alpha float array of planner bias
  #' @param ar_beta string name of the A_i variable, any nonlinear or linear
  #'   evaluated expected outcome
  #' @param fl_rho float preference for equality for the planner, negative
  #'   infinity to 1
  #' @return an float value for resource equivalent variation, see Theorem 1
  #' @author Fan Wang, \url{http://fanwangecon.github.io}
  #' @references
  #' \url{https://fanwangecon.github.io/PrjOptiAlloc/reference/ffp_opt_sobin_rev.html}
  #' \url{https://fanwangecon.github.io/PrjOptiAlloc/articles/ffv_opt_sobin_rev.html}
  #' @export
  #' @examples
  #' library(tibble)
  #' library(dplyr)
  #' library(tidyr)
  #' fl_rho <- -1
  #' ar_alpha <- c(0.1,1.5,2.5,4  ,9,1.2,3,2,8)
  #' ar_A <-     c(0.5,1.5,3.5,6.5,1.9,3,4,6,4)
  #' ar_beta <-  rep(0, length(ar_A)) + 1/length(ar_A)
  #' mt_alpha_A <- cbind(ar_alpha, ar_A, ar_beta)
  #' ar_st_varnames <- c('alpha', 'A', 'beta')
  #' tb_alpha_A <- as_tibble(mt_alpha_A) %>% rename_all(~c(ar_st_varnames))
  #' tb_alpha_A
  #' ar_rho <- c(-1)
  #' for (it_rho_ctr in seq(1,length(ar_rho))) {
  #'   fl_rho <- ar_rho[it_rho_ctr]
  #'   ls_ranks <- ffp_opt_sobin_target_row(tb_alpha_A[1,], fl_rho, ar_A, ar_alpha, ar_beta)
  #'   it_rank <- ls_ranks$it_rank
  #'   ar_it_rank <- ls_ranks$ar_it_rank
  #'   ar_fl_rank_val <- ls_ranks$ar_fl_rank_val
  #'   cat('fl_rho:', fl_rho, 'ar_it_rank:', ar_it_rank, '\n')
  #' }
  #' ar_bin_observed <- c(0,1,0,1,0,0,1,1,0)
  #' ar_queue_optimal <- ar_it_rank
  #' ffp_opt_sobin_rev(ar_queue_optimal, ar_bin_observed, ar_A, ar_alpha, ar_beta, fl_rho)

  # Set up Data Struture
  mt_rev <- cbind(
    seq(1, length(ar_queue_optimal)), ar_bin_observed, ar_queue_optimal,
    ar_A, ar_alpha, ar_beta
  )
  ar_st_varnames <- c("id", "observed", "optimal", "A", "alpha", "beta")
  tb_onevar <- as_tibble(mt_rev) %>%
    rename_all(~ c(ar_st_varnames)) %>%
    arrange(optimal)

  # Generate util observed and util unobserved columns
  tb_onevar <- tb_onevar %>%
    mutate(utility = beta * ((A + alpha)^fl_rho - A^fl_rho)) %>%
    mutate(util_ob = case_when(
      observed == 1 ~ utility,
      TRUE ~ 0
    )) %>%
    mutate(util_notob = case_when(
      observed == 0 ~ utility,
      TRUE ~ 0
    ))

  # Generate directional cumulative sums
  tb_onevar <- tb_onevar %>%
    arrange(-optimal) %>%
    mutate(util_ob_sum = cumsum(util_ob)) %>%
    arrange(optimal) %>%
    mutate(util_notob_sum = cumsum(util_notob))

  # Following Theorem 1, simply compare util_ob_sum vs util_notob_sum
  tb_onevar <- tb_onevar %>%
    mutate(
      opti_better_obs =
        case_when(
          (util_notob_sum / util_ob_sum > 1) ~ 1,
          TRUE ~ 0
        )
    )

  # Find the Rank number tha tmatches the First 1 in opti_better_obs
  tb_onevar <- tb_onevar %>%
    mutate(opti_better_obs_cumu = cumsum(opti_better_obs)) %>%
    mutate(
      min_w_hat =
        case_when(
          opti_better_obs_cumu == 1 ~ optimal,
          TRUE ~ 0
        )
    )

  # Compute REV
  it_min_w_hat <- max(tb_onevar %>% pull(min_w_hat))
  it_w_hat_obs <- sum(ar_bin_observed)
  delta_rev <- 1 - (it_min_w_hat / it_w_hat_obs)

  # Return
  return(delta_rev)
}