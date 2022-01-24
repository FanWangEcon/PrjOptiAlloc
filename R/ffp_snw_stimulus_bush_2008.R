ffp_snw_tax_liability <- function(ar_income = seq(0, 20000, length.out = 5), bl_verbose = FALSE) {
  #' US 2008 Tax liability by household type given income for Nygaard, Sorernsen
  #' and Wang (2021)
  #'
  #' @description We can study the effects of the 2008 Tax Rebate. The Tax
  #'   rebate is a rebate based on how much tax was paid, so we need to know
  #'   taxable income and tax liability. These differ by income, household
  #'   marital status, and the count of children.
  #'
  #'   Given an array of pre-tax income values, we compute for from 0 to 4 kids
  #'   and both married and unmarried taxable-income and tax-liability at all
  #'   points along the income array.
  #'
  #'   Deductions are from 2008 income
  #'   \url{https://www.irs.gov/pub/irs-prior/f1040--2008.pdf}. Tax brackets
  #'   from 2008 are here
  #'   \url{https://www.irs.gov/pub/irs-prior/i1040tt--2008.pdf}.
  #'
  #' @param ar_income array of income points over which to evaluate taxable
  #'   income and also tax liability by kids count and marital status.
  #'
  #' @return a list of 3-d arrays \itemize{ \item mn_taxable_income - 2008
  #'   taxable income given income 3d array where 1st dimension is income, 2nd
  #'   dimension is marital status, 3rd dimension is kids count \item
  #'   mn_tax_liability - 2008 taxable liability given income 3d array }
  #'
  #' @author Fan Wang, \url{http://fanwangecon.github.io}
  #' @export
  #' @references
  #' \url{https://fanwangecon.github.io/PrjOptiAlloc/articles/ffv_snw_stimulus_bush_2008.html}
  #' @examples
  #' ar_income <- c(1e4, 2e4, 4e4, 8e4, 1.6e5)
  #' ls_taxable <- ffp_snw_tax_liability(ar_income)
  #' mn_taxable_income <- ls_taxable$mn_taxable_income
  #' mn_tax_liability <- ls_taxable$mn_tax_liability
  #' print('mn_taxable_income')
  #' print(mn_taxable_income)
  #' print('mn_tax_liability')
  #' print(mn_tax_liability)
  #'

  # A. Exemptions and Deductions
  fl_exemption <- 3500 # exemption amount per household member

  mt_deduction <- matrix(data = NA, nrow = 2, ncol = 5) # Marital-status and number of children-specific deduction
  mt_deduction[1, 1] <- 5450 # Single filers
  mt_deduction[1, 2:5] <- 8000 # Single filer with children
  mt_deduction[2, ] <- 10900 # Married couples filing jointly

  # B. Taxable Income
  # income = [70000];
  mn_taxable_income <- array(rep(NA, length(ar_income) * 2 * 5), dim = c(length(ar_income), 2, 5))

  for (y in 1:length(ar_income)) {
    for (m in 1:2) {
      for (k in 0:4) {
        mn_taxable_income[y, m, k + 1] <- ar_income[y] - fl_exemption * m - fl_exemption * k - mt_deduction[m, k + 1]
      }
    }
  }

  # C. Tax Liability
  mn_tax_liability <- array(rep(NA, length(ar_income) * 2 * 5), dim = c(length(ar_income), 2, 5))

  # Single filers with 0 children
  for (y in 1:length(ar_income)) {
    if (mn_taxable_income[y, 1, 1] <= 8025) {
      mn_tax_liability[y, 1, 1] <- 0.1 * mn_taxable_income[y, 1, 1]
    } else if (mn_taxable_income[y, 1, 1] > 8025 && mn_taxable_income[y, 1, 1] <= 32550) {
      mn_tax_liability[y, 1, 1] <- 0.1 * 8025 + 0.15 * (mn_taxable_income[y, 1, 1] - 8025)
    } else if (mn_taxable_income[y, 1, 1] > 32550 && mn_taxable_income[y, 1, 1] <= 78850) {
      mn_tax_liability[y, 1, 1] <- 0.1 * 8025 + 0.15 * (32550 - 8025) + 0.25 * (mn_taxable_income[y, 1, 1] - 32550)
    } else if (mn_taxable_income[y, 1, 1] > 78850 && mn_taxable_income[y, 1, 1] <= 164550) {
      mn_tax_liability[y, 1, 1] <- 0.1 * 8025 + 0.15 * (32550 - 8025) + 0.25 * (78850 - 32550) + 0.28 * (mn_taxable_income[y, 1, 1] - 78850)
    } else if (mn_taxable_income[y, 1, 1] > 164550 && mn_taxable_income[y, 1, 1] <= 357700) {
      mn_tax_liability[y, 1, 1] <- 0.1 * 8025 + 0.15 * (32550 - 8025) + 0.25 * (78850 - 32550) + 0.28 * (164550 - 78850) + 0.33 * (mn_taxable_income[y, 1, 1] - 164550)
    } else if (mn_taxable_income[y, 1, 1] > 357700) {
      mn_tax_liability[y, 1, 1] <- 0.1 * 8025 + 0.15 * (32550 - 8025) + 0.25 * (78850 - 32550) + 0.28 * (164550 - 78850) + 0.33 * (357700 - 164550) + 0.35 * (mn_taxable_income[y, 1, 1] - 357700)
    }
  }


  # Single filers with 1-4 children
  for (k in 2:5) {
    for (y in 1:length(ar_income)) {
      if (mn_taxable_income[y, 1, k] <= 11450) {
        mn_tax_liability[y, 1, k] <- 0.1 * mn_taxable_income[y, 1, k]
      } else if (mn_taxable_income[y, 1, k] > 11450 && mn_taxable_income[y, 1, k] <= 43650) {
        mn_tax_liability[y, 1, k] <- 0.1 * 11450 + 0.15 * (mn_taxable_income[y, 1, k] - 11450)
      } else if (mn_taxable_income[y, 1, k] > 43650 && mn_taxable_income[y, 1, k] <= 112650) {
        mn_tax_liability[y, 1, k] <- 0.1 * 11450 + 0.15 * (43650 - 11450) + 0.25 * (mn_taxable_income[y, 1, k] - 43650)
      } else if (mn_taxable_income[y, 1, k] > 112650 && mn_taxable_income[y, 1, k] <= 182400) {
        mn_tax_liability[y, 1, k] <- 0.1 * 11450 + 0.15 * (43650 - 11450) + 0.25 * (112650 - 43650) + 0.28 * (mn_taxable_income[y, 1, k] - 112650)
      } else if (mn_taxable_income[y, 1, k] > 182400 && mn_taxable_income[y, 1, k] <= 357700) {
        mn_tax_liability[y, 1, k] <- 0.1 * 11450 + 0.15 * (43650 - 11450) + 0.25 * (112650 - 43650) + 0.28 * (182400 - 112650) + 0.33 * (mn_taxable_income[y, 1, k] - 182400)
      } else if (mn_taxable_income[y, 1, k] > 357700) {
        mn_tax_liability[y, 1, k] <- 0.1 * 11450 + 0.15 * (43650 - 11450) + 0.25 * (112650 - 43650) + 0.28 * (182400 - 112650) + 0.33 * (357700 - 182400) + 0.35 * (mn_taxable_income[y, 1, k] - 357700)
      }
    }
  }


  # Married filers
  for (k in 1:5) {
    for (y in 1:length(ar_income)) {
      if (mn_taxable_income[y, 2, k] <= 16050) {
        mn_tax_liability[y, 2, k] <- 0.1 * mn_taxable_income[y, 2, k]
      } else if (mn_taxable_income[y, 2, k] > 16050 && mn_taxable_income[y, 2, k] <= 65100) {
        mn_tax_liability[y, 2, k] <- 0.1 * 16050 + 0.15 * (mn_taxable_income[y, 2, k] - 16050)
      } else if (mn_taxable_income[y, 2, k] > 65100 && mn_taxable_income[y, 2, k] <= 131450) {
        mn_tax_liability[y, 2, k] <- 0.1 * 16050 + 0.15 * (65100 - 16050) + 0.25 * (mn_taxable_income[y, 2, k] - 65100)
      } else if (mn_taxable_income[y, 2, k] > 131450 && mn_taxable_income[y, 2, k] <= 200300) {
        mn_tax_liability[y, 2, k] <- 0.1 * 16050 + 0.15 * (65100 - 16050) + 0.25 * (131450 - 65100) + 0.28 * (mn_taxable_income[y, 2, k] - 131450)
      } else if (mn_taxable_income[y, 2, k] > 200300 && mn_taxable_income[y, 2, k] <= 357700) {
        mn_tax_liability[y, 2, k] <- 0.1 * 16050 + 0.15 * (65100 - 16050) + 0.25 * (131450 - 65100) + 0.28 * (200300 - 131450) + 0.33 * (mn_taxable_income[y, 2, k] - 200300)
      } else if (mn_taxable_income[y, 2, k] > 357700) {
        mn_tax_liability[y, 2, k] <- 0.1 * 16050 + 0.15 * (65100 - 16050) + 0.25 * (131450 - 65100) + 0.28 * (200300 - 131450) + 0.33 * (357700 - 200300) + 0.35 * (mn_taxable_income[y, 2, k] - 357700)
      }
    }
  }

  for (y in 1:length(ar_income)) {
    for (m in 1:2) {
      for (k in 0:4) {
        mn_tax_liability[y, m, k + 1] <- max(mn_tax_liability[y, m, k + 1], 0)
      }
    }
  }

  # D. Name dimensions
  dimnames(mn_taxable_income)[[1]] <- paste0("income=", round(ar_income, 0))
  dimnames(mn_taxable_income)[[2]] <- paste0("married=", 0:1)
  dimnames(mn_taxable_income)[[3]] <- paste0("kids=", 0:4)
  dimnames(mn_tax_liability)[[1]] <- paste0("income=", round(ar_income, 0))
  dimnames(mn_tax_liability)[[2]] <- paste0("married=", 0:1)
  dimnames(mn_tax_liability)[[3]] <- paste0("kids=", 0:4)

  # E. Print and return
  if (bl_verbose) {
    print(mn_taxable_income)
    print(mn_tax_liability)
  }

  return(list(
    mn_taxable_income = mn_taxable_income,
    mn_tax_liability = mn_tax_liability
  ))
}


ffp_snw_stimulus_checks_bush <- function(ar_income = seq(0, 250000, 5),
                                         it_kids = 0, bl_marital = 0,
                                         fl_stimulus_child = 300,
                                         fl_stimulus_adult_min = 300, fl_stimulus_adult_max = 600,
                                         fl_per_adult_phase_out = 75000,
                                         fl_phase_out_per_dollar_income = 0.05) {
  #' US Economic Recovery Act of 2008 Bush Stimulus Check amounts (tax-rebates)
  #' by household type and income array for Nygaard, Sorernsen and Wang (2021)
  #'
  #' @description Bush checks, not used in matlab, meaning matlab solution
  #'   functions do not call this. There are separate R functions that implement
  #'   the checks that are unrelated to this, this is just to graph out the
  #'   stimulus schedule for visualization
  #'
  #'   Economic Stimulus Act of 2008: 600 ($1,200) for singles (couples) making
  #'   less than $75,000 ($150,000). $300 per child. Amount phases out at a rate
  #'   of 5 percent (drops by $50 per $1,000 in income exceeding $75,000
  #'   ($150,000)).
  #'
  #'   IRS rules for stimulus checks (tax rebates) under the Economic Stimulus
  #'   Act of 2008:
  #'   \url{https://www.irs.gov/newsroom/single-head-of-household-with-children},
  #'    \url{https://www.irs.gov/newsroom/married-with-children},
  #'   \url{https://www.irs.gov/newsroom/married-without-qualifying-children},
  #'   and
  #'   \url{https://www.irs.gov/newsroom/single-without-qualifying-children}.
  #'
  #' @param ar_income array of income points over which to evaluate taxable
  #'   income and also tax liability by kids count and marital status.
  #' @param it_kids integer the number of children from 0 to 4 allowed
  #' @param bl_marital boolean if 1 means married, if 0 means unmarried
  #' @param fl_stimulus_child float the amount of stimulus per child, note that
  #'   this is unrelated to the tax-liability issue.
  #' @param fl_stimulus_adult_min float the amount of minimum stimulus per adult
  #'   (for lower income households), even if tax liability is below this, so no
  #'   tax-rebate should be given, still provide this stimulus amount.
  #' @param fl_stimulus_adult_max float the amount of max stimulus per adult, in
  #'   another word, the maximum amount of tax-rebate.
  #' @param fl_per_adult_phase_out float the amount per-adult (household-head
  #'   only or household-head plus spouse) where phase-out for
  #'   stimulus/tax-rebate begin going down. 75k under the actual policy for
  #'   singles, and 150k for married.
  #' @param fl_phase_out_per_dollar_income for every addition dollar beyond the
  #'   phaseout point, how much stimulus/tax-rebate to reduce. 0.05 means for
  #'   every additional dollar of income, stimulus go down by 5 cents.
  #' @author Fan Wang, \url{http://fanwangecon.github.io}
  #'
  #' @return an array of tax-liabilities for particular kids count and martial
  #'   status along an array of income levels
  #' @references
  #' \url{https://fanwangecon.github.io/PrjOptiAlloc/articles/ffv_snw_stimulus_bush_2008.html}
  #' @export
  #' @examples
  #' ar_income <- c(1e4, 2e4, 4e4, 8e4, 1.6e5)
  #' it_kids <- 0
  #' bl_marital <- 0
  #' ar_stimulus_check <- ffp_snw_stimulus_checks_bush(ar_income, it_kids, bl_marital)
  #' print('ar_stimulus_check')
  #' print(ar_stimulus_check)
  #'

  ls_taxable <- ffp_snw_tax_liability(ar_income)
  mn_taxable_income <- ls_taxable$mn_taxable_income
  mn_tax_liability <- ls_taxable$mn_tax_liability

  kids <- it_kids + 1
  married <- bl_marital + 1
  ar_tax_liability <- mn_tax_liability[, married, it_kids + 1]

  ar_tax_liability <- as.numeric(ar_tax_liability)
  ar_income <- as.numeric(ar_income)

  # stimulus check function first check
  ar_stimulus_check <- pmax(
    pmax(fl_stimulus_adult_min * (1 + bl_marital), pmin(ar_tax_liability, fl_stimulus_adult_max * (1 + bl_marital))) +
      it_kids * fl_stimulus_child -
      pmax(0, (t(ar_income) - fl_per_adult_phase_out * (1 + bl_marital))) * fl_phase_out_per_dollar_income,
    0
  )

  # return
  return(ar_stimulus_check)
}


ffp_snw_stimulus_checks_bush_add2dfid <- function(df_id,
                                                  it_income_n_in_seg = 100,
                                                  fl_multiple = 54831,
                                                  fl_percheck_dollar = 100,
                                                  fl_stimulus_child = 300,
                                                  fl_stimulus_adult_min = 300, fl_stimulus_adult_max = 600,
                                                  fl_per_adult_phase_out = 75000,
                                                  fl_phase_out_per_dollar_income = 0.05) {
  #' Given dataframe solve for stimulus across household types and find relaxed
  #' policy bounds as defined by counterfactuals.
  #'
  #' @description Given dataframe solve for US Economic Recovery Act of 2008 Bush
  #'  Stimulus Check amounts (tax-rebates) by household type and income array
  #'  for Nygaard, Sorernsen and Wang (2021) for each household type. Also
  #'  compute maximum allocation bounds based on tax-liability to consider
  #'  alternatives to the actual stimulus (tax-rebate) policy.
  #'
  #'  We have a dataframe of households, where each household is defined by the
  #'  number of kids in the household, marital status, and also income bin. Note
  #'  that this is an income bin, not a specific income level. We computes an
  #'  approximate income-bin (and marital status and kids count) specific
  #'  stimulus amount by evaluating the stimulus checks function along a fine
  #'  grid of income levels from the min to the max point of the income-bin, and
  #'  simply take the average.
  #'
  #'  We do this first for the actual stimulus that households should receive
  #'  under the Economic Stimulus Act of 2008. We then adjust parameters for the
  #'  stimulus function and compute alternative max-stimulus bounds for each
  #'  income bin.
  #'
  #' @param df_id dataframe the dataframe that is generated at the start of the
  #'  allocation problem, it contains the individual id, marital status, kids
  #'  count, and a string varaible indicating lower and upper bound for current
  #'  household type's income bin.
  #' @param it_income_n_in_seg integer the number of points between min and max
  #'  income bounds for the current income bin at which to evaluate stimulus,
  #'  and to average.
  #' @param fl_multiple integer what is 1 unit in dollar in 2008, equal to 54831,
  #'  median income.
  #' @param fl_percheck_dollar integer we discretize the stimulus problem, how
  #'  much is each stimulus check in dollars?
  #' @param fl_stimulus_child float the amount of stimulus per child, note that
  #'  this is unrelated to the tax-liability issue.
  #' @param fl_stimulus_adult_min float the amount of minimum stimulus per adult
  #'  (for lower income households), even if tax liability is below this, so no
  #'  tax-rebate should be given, still provide this stimulus amount.
  #' @param fl_stimulus_adult_max float the amount of max stimulus per adult, in
  #'  another word, the maximum amount of tax-rebate.
  #' @param fl_per_adult_phase_out float the amount per-adult (household-head
  #'  only or household-head plus spouse) where phase-out for
  #'  stimulus/tax-rebate begin going down. 75k under the actual policy for
  #'  singles, and 150k for married.
  #' @param fl_phase_out_per_dollar_income for every addition dollar beyond the
  #'  phaseout point, how much stimulus/tax-rebate to reduce. 0.05 means for
  #'  every additional dollar of income, stimulus go down by 5 cents.
  #' @author Fan Wang, \url{http://fanwangecon.github.io}
  #'
  #' @return an array of tax-liabilities for particular kids count and martial
  #'  status along an array of income levels
  #' @references
  #' \url{https://fanwangecon.github.io/PrjOptiAlloc/articles/ffv_snw_stimulus_bush_2008.html}
  #' @export
  #' @examples
  #' data(df_nsw_tiny_chk168_df_id)
  #' df_id <- df_nsw_tiny_chk168_df_id
  #' it_income_n_in_seg <- 100
  #' df_id_checkadded_actual <- ffp_snw_stimulus_checks_bush_add2dfid(df_id, it_income_n_in_seg)
  #' print('df_id_checkadded_actual')
  #' print(df_id_checkadded_actual)
  #'

  # Assumed variable names for df_id
  svr_id <- "id_i"
  svr_ymin_group <- "ymin_group"
  svr_marital <- "marital"
  svr_kids <- "kids"

  # First, parse the *ymin_group* group.
  df_id <- df_id %>%
    rowwise() %>%
    mutate(ymin_group = as.character(ymin_group)) %>%
    mutate(
      y_group_min = substring(strsplit(ymin_group, ",")[[1]][1], 2),
      y_group_max = gsub(strsplit(ymin_group, ",")[[1]][2], pattern = "]", replacement = "")
    ) %>%
    mutate(
      y_group_min = fl_multiple * as.numeric(y_group_min),
      y_group_max = fl_multiple * as.numeric(y_group_max)
    ) %>%
    ungroup()

  # Second, generate an income array with *y_group_min* and *y_group_max*, and
  # call the stimulus function to solve for stimulus along the income array, and
  # then take average. Set various parameters
  df_id <- df_id %>%
    group_by(!!sym(svr_id)) %>%
    do(
      bush_rebate =
        mean(ffp_snw_stimulus_checks_bush(
          ar_income = seq(.[["y_group_min"]],
            .[["y_group_max"]],
            length.out = it_income_n_in_seg
          ),
          it_kids = .[[svr_kids]],
          bl_marital = .[[svr_marital]],
          fl_stimulus_child = fl_stimulus_child,
          fl_stimulus_adult_min = fl_stimulus_adult_min,
          fl_stimulus_adult_max = fl_stimulus_adult_max,
          fl_per_adult_phase_out = fl_per_adult_phase_out,
          fl_phase_out_per_dollar_income = fl_phase_out_per_dollar_income
        ))
    ) %>%
    unnest(c(bush_rebate)) %>%
    mutate(bush_rebate_n_checks = round(bush_rebate / fl_percheck_dollar)) %>%
    left_join(df_id, by = svr_id)

  # return
  return(df_id)
}