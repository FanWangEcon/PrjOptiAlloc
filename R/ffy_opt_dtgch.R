ffy_opt_dtgch_cbem4 <- function() {
    #' Generates A and alpha for linear and log linear problems from Guat Cebu 4 Groups
    #'
    #' @description
    #' Given a dataset with Y, x1, xothers. Regress with log linear and linear structures
    #' to obtain A and alpha that the linear and log linear optimal allocation problems understand.
    #' dtgch = data guatemala cebu height. There discrete groups, allowing for alpha to differ.
    #'
    #' @return a list with two tibble with guatemala and cebu dataset only some columns and A and alpha lin loglin
    #' \itemize{
    #'   \item df_raw - Dataframe from Guat/Cebu subsetted Ceb only, 4 categories
    #'   \item df_esti - A dataframe with 5 columns, lin and loglin A, alpha and beta.
    #' }
    #' @author Fan Wang, \url{http://fanwangecon.github.io}
    #' @references
    #' \url{https://fanwangecon.github.io/PrjOptiAlloc/reference/ffy_opt_dtgch_fourg.html}
    #' @export
    #' @import dplyr tidyr stringr broom R4Econ
    #' @examples
    #' ls_opti_alpha_A <- ffy_opt_dtgch_cbem4()
    #' head(ls_opti_alpha_A$df_raw, 10)
    #' head(ls_opti_alpha_A$df_esti, 10)

    ###############################################
    ### A. Data Selection
    ###############################################

    df_hw <- df_hgt_wgt_po %>% filter(S.country == 'Cebu' & svymthRound == 24 & prot > 0 & hgt > 0) %>% drop_na()

    # Generate Discrete Version of momEdu
    df_hw <- df_hw %>%
        mutate(momEduRound = cut(momEdu,
                                 breaks=c(-Inf, 10, Inf),
                                 labels=c("MEduLow","MEduHigh"))) %>%
        mutate(hgt0med = cut(hgt0,
                                 breaks=c(-Inf, 50, Inf),
                                 labels=c("h0low","h0high")))

    df_hw$momEduRound = as.factor(df_hw$momEduRound)
    df_hw$hgt0med = as.factor(df_hw$hgt0med)

    # Attach
    attach(df_hw)

    ###############################################
    ## 2. Linear Regression
    ###############################################

    # Input Matrix
    mt_lincv <- model.matrix(~ hgt0 + wgt0)
    mt_linht <- model.matrix(~ sex:hgt0med - 1)

    # Regress Height At Month 24 on Nutritional Inputs with controls
    rs_hgt_prot_lin = lm(hgt ~ prot:mt_linht + mt_lincv - 1)
    print(summary(rs_hgt_prot_lin))
    rs_hgt_prot_lin_tidy = tidy(rs_hgt_prot_lin)

    ###############################################
    ## 3. Log-Linear Regression
    ###############################################

    # Input Matrix Generation
    mt_logcv <- model.matrix(~ hgt0 + wgt0)
    mt_loght <- model.matrix(~ sex:hgt0med - 1)

    # Log and log regression for month 24
    rs_hgt_prot_log = lm(log(hgt) ~ log(prot):mt_loght + mt_logcv - 1)
    print(summary(rs_hgt_prot_log))
    rs_hgt_prot_log_tidy = tidy(rs_hgt_prot_log)

    ###############################################
    ## 4. Construct Input Arrays $A_i$ and $\alpha_i$
    ###############################################

    # Generate A_i
    ar_Ai_lin <- mt_lincv %*% as.matrix(rs_hgt_prot_lin_tidy %>% filter(!str_detect(term, 'prot')) %>% select(estimate))
    ar_Ai_log <- mt_logcv %*% as.matrix(rs_hgt_prot_log_tidy %>% filter(!str_detect(term, 'prot')) %>% select(estimate))

    # Generate alpha_i
    ar_alphai_lin <- mt_linht %*% as.matrix(rs_hgt_prot_lin_tidy %>% filter(str_detect(term, 'prot')) %>% select(estimate))
    ar_alphai_log <- mt_loght %*% as.matrix(rs_hgt_prot_log_tidy %>% filter(str_detect(term, 'prot')) %>% select(estimate))

    # Child Weight
    ar_beta <- rep(1/length(ar_Ai_lin), times=length(ar_Ai_lin))

    # Initate Dataframe that will store all estimates and optimal allocation relevant information
    mt_opti <- cbind(ar_alphai_lin, ar_alphai_log, ar_Ai_lin, ar_Ai_log, ar_beta)
    ar_st_varnames <- c('alpha_lin', 'alpha_log', 'A_lin', 'A_log', 'beta')
    df_esti_alpha_A_beta <- as_tibble(mt_opti) %>% rename_all(~c(ar_st_varnames))

    ###############################################
    # 5. Unique Elements? Posible deal with uniques (or maybe not necessary) possibly own function
    ###############################################

    # tb_opti_unique <- tb_opti %>% group_by(!!!syms(ar_st_varnames)) %>%
    #                     arrange(!!!syms(ar_st_varnames)) %>%
    #                     summarise(n_obs_group=n())

    ###############################################
    # 5. Return
    ###############################################

    return(list(df_raw = df_hw, df_esti = df_esti_alpha_A_beta))

}
