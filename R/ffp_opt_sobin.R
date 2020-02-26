# alpha and A are positive correlated, the allocation rank will switch
ar_alpha <- c(0.1,1.5,2.5,4)
ar_A <-     c(0.5,1.5,3.5,6.5)
ar_beta <- c(0.25,0.25,0.25,0.25)
mt_alpha_A <- cbind(ar_alpha, ar_A, ar_beta)
ar_st_varnames <- c('alpha', 'A', 'beta')
tb_alpha_A <- as_tibble(mt_alpha_A) %>% rename_all(~c(ar_st_varnames))
tb_alpha_A

ar_rho <- c(-100, -1.1, 0.01, 0.10, 0.9)
for (it_rho_ctr in seq(1,length(ar_rho))) {
  fl_rho <- ar_rho[it_rho_ctr]
  it_rank <- tb_alpha_A %>% rowwise() %>%
    do(rk = ffp_opt_sobin_target_row(., fl_rho,
                                     ar_A, ar_alpha, ar_beta)) %>%
    unnest(rk) %>% pull(rk)

  cat('fl_rho:', fl_rho, 'it_rank:', it_rank, '\n')
}

ffp_opt_sobin_target_row

ffp_opt_sobin_target_row <- function(ls_row, fl_rho,
                                     ar_A, ar_alpha, ar_beta,
                                     svr_A_i = 'A', svr_alpha_i = 'alpha', svr_beta_i = 'beta'){
  #' Solves the binary targeting queue with Ai_i, alpha_i, and planner preferences, for one individual
  #'
  #' @description
  #' This does not solve for the full solution, just the targeting queue, individual position on queue.
  #'
  #' @param ls_row list a row from dataframe tibble row where there are variables for A, alpha and beta
  #' @param ar_A float array of expected outcome without provision for all N
  #' @param ar_alpha float array of expected effect of provision for each i of N
  #' @param ar_beta float array of planner bias
  #' @param svr_A_i string name of the A_i variable, any nonlinear or linear evaluated expected outcome
  #' @param svr_alpha_i string name of the alpha_i variable, individual specific effects of treatment
  #' @param svr_beta_i string name of the beta_i variable, relative preference weight for each child
  #' @param fl_rho float preference for equality for the planner
  #' @return an integer equal to the person's rank on the optimal binary targeting queue, smaller number means higher ranking, 1 is ranked first to receive allocations
  #' @author Fan Wang, \url{http://fanwangecon.github.io}
  #' @references
  #' \url{https://fanwangecon.github.io/PrjOptiAlloc/reference/ffp_opt_solin_relow.html}
  #' \url{https://fanwangecon.github.io/PrjOptiAlloc/articles/ffv_opt_solin_relow.html}
  #' @export
  #' @import
  #' @examples
  #' ls_row =
  #' df <- df_opt_dtgch_cbem4
  #' svr_A_i <- 'A_lin'
  #' svr_alpha_i <- 'alpha_lin'
  #' svr_beta_i <- 'beta'
  #' fl_N_agg <- 10000
  #' fl_rho <- -30
  #' ls_lin_solu <- ffp_opt_solin_relow(df, svr_A_i, svr_alpha_i, svr_beta_i, fl_N_agg, fl_rho)
  #' df_opti <- ls_lin_solu$df_opti
  #' ar_opti_inpalc <- ls_lin_solu$ar_opti_inpalc
  #' ar_opti_expout <- ls_lin_solu$ar_opti_expout
  #' summary(df_opti)
  #' print(ar_opti_inpalc)
  #' print(ar_opti_expout)

  fl_alpha <- ls_row[[svr_alpha_i]]
  fl_A <- ls_row[[svr_A_i]]
  fl_beta <- ls_row[[svr_beta_i]]

  ar_left <- (((ar_A + ar_alpha)^fl_rho - (ar_A)^fl_rho)
              /
              ((fl_A + fl_alpha)^fl_rho - (fl_A)^fl_rho))
  ar_right <- ((ar_beta)/(fl_beta))
  ar_full <- ar_left*ar_right
  print('ar_full')
  print(ar_full)
  ar_indicator <- (ar_full >= 1)

  it_rank <- sum(ar_indicator)

  return(it_rank)
}

ffp_opt_solin_relow <- function(df, svr_A_i, svr_alpha_i, svr_beta_i, fl_N_agg,
                                fl_rho,
                                svr_inpalc = 'optiallocate',
                                svr_expout = 'optiexpoutcm') {
#' Solves the linear allocation problem with Ai_i, alpha_i, and planner preferences
#'
#' @description
#' This is the solution to the linear optimal allocation problem. Relative allocations, summed splined, inversion.
#' solin, solution linear. relow, relative to the lowest allocation algoritm.
#'
#' @param df tibble data table including variables using svr names below each row is potentially an individual who will receive alternative allocations
#' @param svr_A_i string name of the A_i variable, dot product of covariates and coefficients
#' @param svr_alpha_i string name of the alpha_i variable, individual specific elasticity information
#' @param svr_beta_i string name of the beta_i variable, relative preference weight for each child
#' @param fl_N_agg float total resourcei avaible for allocation, if not specific, sum by svr_N_i
#' @param fl_rho float preference for equality for the planner
#' @param svr_inpalc string variable name for newly generated input optimal allocation, single word no dash
#' @param svr_expout string variable name for newly generated expected outcome, single word no dash
#' @return a dataframe that expands the df inputs with additional results.
#' @return a list with a dataframe and an array
#' \itemize{
#'   \item df_opti - Dataframe with various statistcs related to optimal allocation, all intermediary stats
#'   \item ar_opti_inpalc - Array where each element is an optimal choice for each individual
#'   \item ar_opti_expout - Array where each element is the expected outcome given optimal choices for each i
#' }
#' @author Fan Wang, \url{http://fanwangecon.github.io}
#' @references
#' \url{https://fanwangecon.github.io/PrjOptiAlloc/reference/ffp_opt_solin_relow.html}
#' \url{https://fanwangecon.github.io/PrjOptiAlloc/articles/ffv_opt_solin_relow.html}
#' @export
#' @import dplyr tidyr stringr broom REconTools
#' @examples
#' data(df_opt_dtgch_cbem4)
#' df <- df_opt_dtgch_cbem4
#' svr_A_i <- 'A_lin'
#' svr_alpha_i <- 'alpha_lin'
#' svr_beta_i <- 'beta'
#' fl_N_agg <- 10000
#' fl_rho <- -30
#' ls_lin_solu <- ffp_opt_solin_relow(df, svr_A_i, svr_alpha_i, svr_beta_i, fl_N_agg, fl_rho)
#' df_opti <- ls_lin_solu$df_opti
#' ar_opti_inpalc <- ls_lin_solu$ar_opti_inpalc
#' ar_opti_expout <- ls_lin_solu$ar_opti_expout
#' summary(df_opti)
#' print(ar_opti_inpalc)
#' print(ar_opti_expout)

ffi_binary_dplyrdo_func <- function(ls_row, fl_rho, bl_old=FALSE){
  # @param bl_old, weather to use old incorrect solution
  # hard coded inputs are:
  # 1, ar_A
  # 2, ar_alpha
  # 3, ar_beta
  # note follow https://fanwangecon.github.io/R4Econ/support/function/fs_applysapplymutate.html

  fl_alpha <- ls_row$alpha
  fl_A <- ls_row$A
  fl_beta <- ls_row$beta

  ar_left <- (
              ((ar_A + ar_alpha)^fl_rho - (ar_A)^fl_rho)
              /
              ((fl_A + fl_alpha)^fl_rho - (fl_A)^fl_rho)
             )
  ar_right <- ((ar_beta)/(fl_beta))
  ar_full <- ar_left*ar_right
  ar_indicator <- (ar_full >= 1)

  it_rank <- sum(ar_indicator)

  return(it_rank)
}
# Returns ----
return(list(df_opti = df_opti, ar_opti_inpalc = ar_opti_inpalc, ar_opti_expout = ar_opti_expout))

}
