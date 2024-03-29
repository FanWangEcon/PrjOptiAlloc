ffp_opt_solin_relow <- function(df, svr_A_i, svr_alpha_i, svr_beta_i, fl_N_agg,
                                fl_rho,
                                svr_inpalc = "optiallocate",
                                svr_expout = "optiexpoutcm") {
   #' Theorem 1, Binary Optimal Allocation solution, for one planner inequality
   #' preference (one rho value)
   #'
   #' @description This is the solution to the linear optimal allocation problem.
   #' Relative allocations, summed splined, inversion. solin, solution linear.
   #' relow, relative to the lowest allocation algoritm.
   #'
   #' @param df tibble data table including variables using svr names below each
   #'   row is potentially an individual who will receive alternative allocations
   #' @param svr_A_i string name of the A_i variable, dot product of covariates and
   #'   coefficients
   #' @param svr_alpha_i string name of the alpha_i variable, individual specific
   #'   marginal-effects
   #' @param svr_beta_i string name of the beta_i variable, relative preference
   #'   weight for each child
   #' @param fl_N_agg float total resource avaible for allocation, if not specific,
   #'   sum by svr_N_i
   #' @param fl_rho float preference for equality for the planner
   #' @param svr_inpalc string variable name for newly generated input optimal
   #'   allocation, single word no dash
   #' @param svr_expout string variable name for newly generated expected outcome,
   #'   single word no dash
   #' @return a dataframe that expands the df inputs with additional results.
   #' @return a list with a dataframe and an array \itemize{ \item df_opti -
   #'   Dataframe with various statistcs related to optimal allocation, all
   #'   intermediary stats \item ar_opti_inpalc - Array where each element is an
   #'   optimal choice for each individual \item ar_opti_expout - Array where each
   #'   element is the expected outcome given optimal choices for each i }
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

   # A. select only relevant data ----
   df_opti <- df %>% rename(A = !!sym(svr_A_i), alpha = !!sym(svr_alpha_i), beta = !!sym(svr_beta_i))

   # B. Generate V4, Rank Index Value, rho specific ----
   # df_opti <- df_opti %>% mutate(!!paste0('rv_', it_rho_ctr) := A/((alpha*beta))^(1/(1-fl_rho)))
   df_opti <- df_opti %>% mutate(rank_val = A / ((alpha * beta))^(1 / (1 - fl_rho)))

   # C. Generate Rank Index ----
   df_opti <- df_opti %>%
      arrange(rank_val) %>%
      mutate(rank_idx = row_number())

   # D. Populate lowest index alpha, beta, and A to all rows ----
   df_opti <- df_opti %>%
      mutate(lowest_rank_A = A[rank_idx == 1]) %>%
      mutate(lowest_rank_alpha = alpha[rank_idx == 1]) %>%
      mutate(lowest_rank_beta = beta[rank_idx == 1])

   # E. relative slope and relative intercept with respect to lowest index ----
   df_opti <- df_opti %>%
      mutate(
         rela_slope_to_lowest =
            (((lowest_rank_alpha * lowest_rank_beta) / (alpha * beta))^(1 / (fl_rho - 1)) * (lowest_rank_alpha / alpha))
      ) %>%
      mutate(
         rela_intercept_to_lowest =
            ((((lowest_rank_alpha * lowest_rank_beta) / (alpha * beta))^(1 / (fl_rho - 1)) * (lowest_rank_A / alpha)) - (A / alpha))
      )

   # F. cumulative sums ----
   df_opti <- df_opti %>%
      mutate(
         rela_slope_to_lowest_cumsum =
            cumsum(rela_slope_to_lowest)
      ) %>%
      mutate(
         rela_intercept_to_lowest_cumsum =
            cumsum(rela_intercept_to_lowest)
      )

   # G. inverting cumulative slopes and intercepts ----
   df_opti <- df_opti %>%
      mutate(
         rela_slope_to_lowest_cumsum_invert =
            (1 / rela_slope_to_lowest_cumsum)
      ) %>%
      mutate(
         rela_intercept_to_lowest_cumsum_invert =
            ((-1) * (rela_intercept_to_lowest_cumsum) / (rela_slope_to_lowest_cumsum))
      )

   # H. Relative x-intercept points ----
   df_opti <- df_opti %>%
      mutate(
         rela_x_intercept =
            (-1) * (rela_intercept_to_lowest / rela_slope_to_lowest)
      )

   # I. Inverted relative x-intercepts ----
   df_opti <- df_opti %>%
      mutate(
         opti_lowest_spline_knots =
            (rela_intercept_to_lowest_cumsum + rela_slope_to_lowest_cumsum * rela_x_intercept)
      )

   # J. Sort by order of receiving transfers/subsidies ----
   df_opti <- df_opti %>% arrange(rela_x_intercept)

   # K. Find position of subsidy ----
   df_opti <- df_opti %>%
      arrange(opti_lowest_spline_knots) %>%
      mutate(tot_devi = opti_lowest_spline_knots - fl_N_agg) %>%
      arrange((-1) * case_when(tot_devi < 0 ~ tot_devi)) %>%
      mutate(
         allocate_lowest =
            case_when(row_number() == 1 ~
            rela_intercept_to_lowest_cumsum_invert +
               rela_slope_to_lowest_cumsum_invert * fl_N_agg)
      ) %>%
      mutate(allocate_lowest = allocate_lowest[row_number() == 1]) %>%
      mutate(!!sym(svr_inpalc) :=
         rela_intercept_to_lowest +
         rela_slope_to_lowest * allocate_lowest) %>%
      mutate(!!sym(svr_inpalc) :=
         case_when(!!sym(svr_inpalc) >= 0 ~ !!sym(svr_inpalc), TRUE ~ 0)) %>%
      mutate(allocate_total = sum(!!sym(svr_inpalc), na.rm = TRUE))

   # L. Predictes Expected choice: A + alpha*opti_allocate ----
   df_opti <- df_opti %>% mutate(!!sym(svr_expout) := A + alpha * !!sym(svr_inpalc))

   # M. Drop some variables that I do not want to keep even in full df to export ----
   # inpalc = input allocation optimal
   # expout = expected outcome given input allocation
   df_opti <- df_opti %>% select(-one_of(c("lowest_rank_alpha", "lowest_rank_beta")))
   ar_opti_inpalc <- df_opti %>% pull(!!sym(svr_inpalc))
   ar_opti_expout <- df_opti %>% pull(!!sym(svr_expout))

   # Returns ----
   return(list(df_opti = df_opti, ar_opti_inpalc = ar_opti_inpalc, ar_opti_expout = ar_opti_expout))
}