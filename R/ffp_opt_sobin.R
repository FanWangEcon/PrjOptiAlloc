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
  #'   it_rank <- tb_alpha_A %>% rowwise() %>%
  #'     do(rk = ffp_opt_sobin_target_row(., fl_rho,
  #'                                      ar_A, ar_alpha, ar_beta)) %>%
  #'     unnest(rk) %>% pull(rk)
  #'
  #'   cat('fl_rho:', fl_rho, 'it_rank:', it_rank, '\n')
  #' }
  #'

  fl_alpha <- ls_row[[svr_alpha_i]]
  fl_A <- ls_row[[svr_A_i]]
  fl_beta <- ls_row[[svr_beta_i]]

  ar_left <- (((ar_A + ar_alpha)^fl_rho - (ar_A)^fl_rho)
              /
              ((fl_A + fl_alpha)^fl_rho - (fl_A)^fl_rho))
  ar_right <- ((ar_beta)/(fl_beta))
  ar_full <- ar_left*ar_right

  ar_indicator <- (ar_full >= 1)

  it_rank <- sum(ar_indicator)

  # message(paste0('it_rank:', it_rank))
  # message(ar_full)

  return(it_rank)
}
