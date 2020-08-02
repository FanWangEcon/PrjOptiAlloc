ffp_snw_graph_optimal <- function(ar_rho, df_input_il_noninc_covar,
                                  df_alloc_i_long_covar_c, df_alloc_i_long_covar_v,
                                  ls_st_gen_imgs = c('mass', 'mc', 'checks_c', 'mv', 'checks_v'),
                                  ls_st_save_imgs = c('mass', 'mc', 'checks_c', 'mv', 'checks_v'),
                                  st_file_type = '',
                                  slb_add_title = '',
                                  stg_subtitle='',
                                  stg_caption='',
                                  st_img_suffix='',
                                  bl_save_img=FALSE,
                                  it_img_width = 135, it_img_height = 96,
                                  st_img_units='mm', it_img_res = 300, it_img_pointsize = 7,
                                  spt_img_save='C:/Users/fan/Documents/Dropbox (UH-ECON)/PrjNygaardSorensenWang/Results/2020-07-29/Graphs/') {
  #' Nygaard, Sorernsen and Wang (2020) Optimal Allocation by household type graphs.
  #'
  #' @description
  #' SNW optimal Graphs
  #'
  #' @author Fan Wang, \url{http://fanwangecon.github.io}
  #' @export
  #'

  # Labels for Kids Count and Marital Status
  marry_levels <- c(single = "0", married = "1")
  kids_levels <- c(no_kids = "0", one_kids = "1",
                   two_kids = "2", three_kids = "3",
                   three_kids = "3", four_kids = "4")

  ## Data Changes
  df_input_il_noninc_covar <- df_input_il_noninc_covar %>%
    mutate(ymin_group = as.numeric(ymin_group),
           kids = as.factor(kids),
           marital = as.factor(marital)) %>%
    mutate(kids = fct_recode(kids, !!!kids_levels),
           marital = fct_recode(marital, !!!marry_levels))

  df_alloc_i_long_covar_c <- df_alloc_i_long_covar_c %>%
    filter(allocate_type == 'optimal') %>%
    filter(rho_val == ar_rho[1]) %>% ungroup() %>%
    mutate(ymin_group = as.numeric(ymin_group),
           kids = as.factor(kids),
           marital = as.factor(marital)) %>%
    mutate(kids = fct_recode(kids, !!!kids_levels),
           marital = fct_recode(marital, !!!marry_levels))

  df_alloc_i_long_covar_v <- df_alloc_i_long_covar_v %>%
    filter(allocate_type == 'optimal') %>%
    filter(rho_val == ar_rho[1]) %>% ungroup() %>%
    mutate(ymin_group = as.numeric(ymin_group),
           kids = as.factor(kids),
           marital = as.factor(marital)) %>%
    mutate(kids = fct_recode(kids, !!!kids_levels),
           marital = fct_recode(marital, !!!marry_levels))


  ## ---- Image Return List
  ls_img <- vector(mode = "list", length = 0)

  ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
  # graph mean check amount by income, marital status and kids counts
  if ('mass' %in% ls_st_gen_imgs) {

    ls_img$mass <- df_alloc_i_long_covar_c %>%
      ggplot(aes(x=ymin_group, y=mass ,
                 colour=age_group,
                 shape=marital)) +
      facet_wrap( ~ marital + kids, ncol=5) +
      geom_point(size=3) +
      labs(title = paste0(slb_add_title, 'Mass At Each Kids/Marry/Income Points, ',
                          st_file_type),
           subtitle = stg_subtitle,
           x = 'Income Group',
           y = 'Mass for this Group',
           caption = stg_caption)

  }

  ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
  if ('mc' %in% ls_st_gen_imgs) {

    # graph mean check amount by income, marital status and kids counts
    ls_img$mc <- df_input_il_noninc_covar %>%
      ggplot(aes(x=ymin_group, y=log(c_alpha_il),
                 colour=age_group,
                 shape=kids)) +
      facet_wrap( ~ marital + kids, ncol=5) +
      geom_point(size=1) +
      # scale_y_continuous(trans='log') +
      labs(title = paste0(slb_add_title,
                          'Marginal Consumption Gain Each Check, Conditional on: Age+Marry+Kids+Income, ',
                          st_file_type),
           subtitle = stg_subtitle,
           x = 'Income Group',
           y = 'Log(Marginal Consumption Gain Per Check)',
           caption = stg_caption)

  }

  ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
  if ('checks_c' %in% ls_st_gen_imgs) {

    # graph mean check amount by income, marital status and kids counts
    ls_img$checks_c <- df_alloc_i_long_covar_c %>%
      ggplot(aes(x=ymin_group, y=checks,
                 colour=age_group,
                 shape=kids,
                 linetype=marital)) +
      facet_wrap( ~ marital + kids, ncol=5) +
      geom_point(size=3) +
      geom_line() +
      labs(title = paste0(slb_add_title,
                          'Optimize Expected 2020 Consumption, Conditional on: Age+Marry+Kids+Income, ',
                          st_file_type),
           subtitle = stg_subtitle,
           x = 'Income Group',
           y = 'Number of Checks',
           caption = stg_caption)

  }

  ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
  if ('mv' %in% ls_st_gen_imgs) {

    # graph mean check amount by income, marital status and kids counts
    ls_img$mv <- df_input_il_noninc_covar %>%
      ggplot(aes(x=ymin_group, y=log(v_alpha_il),
                 colour=age_group,
                 shape=kids)) +
      facet_wrap( ~ marital + kids, ncol=5) +
      geom_point(size=1) +
      labs(title = paste0(slb_add_title,
                          'Marginal Value Gain Each Check, Conditional on: Age+Marry+Kids+Income, ',
                          st_file_type),
           subtitle = stg_subtitle,
           x = 'Income Group',
           y = 'Log(Marginal Value Gain Per Check)',
           caption = stg_caption)

  }

  ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
  if ('checks_v' %in% ls_st_gen_imgs) {

    # graph mean check amount by income, marital status and kids counts
    ls_img$checks_v <- df_alloc_i_long_covar_v %>%
      filter(allocate_type == 'optimal') %>%
      filter(rho_val == ar_rho[1]) %>% ungroup() %>%
      mutate(ymin_group = as.numeric(ymin_group),
             kids = as.factor(kids),
             marital = as.factor(marital)) %>%
      ggplot(aes(x=ymin_group, y=checks,
                 colour=age_group,
                 shape=kids,
                 linetype=marital)) +
      facet_wrap( ~ marital + kids, ncol=5) +
      geom_point(size=3) +
      geom_line() +
      labs(title =
             paste0(slb_add_title,
                    'Optimize Expected 2020 Value, Conditional on: Age+Marry+Kids+Income, ',
                    st_file_type),
           subtitle = stg_subtitle,
           x = 'Income Group',
           y = 'Number of Checks',
           caption = stg_caption)

  }

  ## ---- Save Graphs

  ar_st_img_names <- names(ls_img)
  if (bl_save_img) {
    for (it_img_ctr in seq(1,length(ar_st_img_names))) {

      # Image Name
      st_img_name <- ar_st_img_names[it_img_ctr]
      st_img_name_full <- paste0(st_img_suffix, '_', st_img_name, '.png')

      if(st_img_name %in% ls_st_save_imgs) {
        # PNG and print
        png(paste0(spt_img_save, st_img_name_full),
            width = it_img_width,
            height = it_img_height, units=st_img_units,
            res = it_img_res, pointsize=it_img_pointsize)
        print(ls_img[it_img_ctr])
        dev.off()
      }

    }
  }

  # Complete and Return
  return(ls_img)

}


ffp_snw_graph_feasible <- function(ar_rho, df_input_il_noninc_covar,
                                   df_alloc_i_long_covar_c, df_alloc_i_long_covar_v,
                                   ls_st_gen_imgs = c('mass', 'mc', 'checks_c', 'mv', 'checks_v'),
                                   ls_st_save_imgs = c('mass', 'mc', 'checks_c', 'mv', 'checks_v'),
                                   st_file_type = '',
                                   slb_add_title = '',
                                   stg_subtitle='', stg_caption='',
                                   st_img_suffix='',
                                   bl_save_img=FALSE,
                                   it_img_width = 135, it_img_height = 96,
                                   st_img_units='mm', it_img_res = 300, it_img_pointsize = 7,
                                   spt_img_save='C:/Users/fan/Documents/Dropbox (UH-ECON)/PrjNygaardSorensenWang/Results/2020-07-29/Graphs/') {

  #' Feasible Graphs considering ages
  #'
  #' @description
  #' SNW feasible Graphs considering ages
  #'
  #' @author Fan Wang, \url{http://fanwangecon.github.io}
  #' @export
  #'

  # Labels for Kids Count and Marital Status
  marry_levels <- c(single = "0", married = "1")
  kids_levels <- c(no_kids = "0", one_kids = "1",
                   two_kids = "2", three_kids = "3",
                   three_kids = "3", four_kids = "4")

  ## Select, as factor, and recode
  df_input_il_noninc_covar <- df_input_il_noninc_covar %>%
    mutate(ymin_group = as.numeric(ymin_group),
           kids = as.factor(kids),
           marital = as.factor(marital)) %>%
    mutate(kids = fct_recode(kids, !!!kids_levels),
           marital = fct_recode(marital, !!!marry_levels))

  df_alloc_i_long_covar_c <- df_alloc_i_long_covar_c %>%
    filter(rho_val == ar_rho[1]) %>% ungroup() %>%
    mutate(ymin_group = as.numeric(ymin_group),
           kids = as.factor(kids),
           marital = as.factor(marital)) %>%
    mutate(kids = fct_recode(kids, !!!kids_levels),
           marital = fct_recode(marital, !!!marry_levels))

  df_alloc_i_long_covar_v <- df_alloc_i_long_covar_v %>%
    filter(rho_val == ar_rho[1]) %>% ungroup() %>%
    mutate(ymin_group = as.numeric(ymin_group),
           kids = as.factor(kids),
           marital = as.factor(marital)) %>%
    mutate(kids = fct_recode(kids, !!!kids_levels),
           marital = fct_recode(marital, !!!marry_levels))

  ## ---- Image Return List
  ls_img <- vector(mode = "list", length = 0)

  ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
  if ('mass' %in% ls_st_gen_imgs) {

    # graph mean check amount by income, marital status and kids counts
    ls_img$mass <- df_alloc_i_long_covar_c %>%
      filter(rho_val == ar_rho[1]) %>% ungroup() %>%
      # filter(marital == 1) %>%
      mutate(ymin_group = as.numeric(ymin_group),
             kids = as.factor(kids),
             marital = as.factor(marital)) %>%
      ggplot(aes(x=ymin_group, y=mass ,
                 colour=age_group,
                 shape=marital)) +
      facet_wrap( ~ marital + kids, ncol=5) +
      geom_point(size=3) +
      labs(title = paste0(slb_add_title, 'Mass At Each Kids/Marry/Income Points, ',
                          st_file_type),
           subtitle = stg_subtitle,
           x = 'Income Group',
           y = 'Mass for this Group',
           caption = stg_caption)

  }

  ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
  if ('mc' %in% ls_st_gen_imgs) {

    # graph mean check amount by income, marital status and kids counts
    ls_img$mc <- df_input_il_noninc_covar %>%
      ggplot(aes(x=ymin_group, y=log(c_alpha_il),
                 colour=checks,
                 shape=kids)) +
      facet_wrap( ~ marital + kids, ncol=5) +
      geom_point(size=1) +
      # scale_y_continuous(trans='log') +
      labs(title = paste0(slb_add_title,
                          'Marginal Consumption Gain Each Check, Conditional on: Marry+Kids+Income, ',
                          st_file_type),
           subtitle = stg_subtitle,
           x = 'Income Group',
           y = 'Log(Marginal Consumption Gain Per Check)',
           caption = stg_caption)
  }

  ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
  if ('checks_c' %in% ls_st_gen_imgs) {

    # graph mean check amount by income, marital status and kids counts
    ls_img$checks_c <- df_alloc_i_long_covar_c %>%
      filter(rho_val == ar_rho[1]) %>% ungroup() %>%
      mutate(ymin_group = as.numeric(ymin_group),
             kids = as.factor(kids),
             marital = as.factor(marital)) %>%
      ggplot(aes(x=ymin_group, y=checks,
                 colour=allocate_type,
                 shape=kids,
                 linetype=marital)) +
      facet_wrap( ~ marital + kids, ncol=5) +
      geom_point(size=3) +
      geom_line() +
      labs(title = paste0(slb_add_title,
                          'Optimize Expected 2020 Consumption, Conditional on: Marry+Kids+Income, ',
                          st_file_type),
           subtitle = stg_subtitle,
           x = 'Income Group',
           y = 'Number of Checks',
           caption = stg_caption)

  }
  ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
  if ('mv' %in% ls_st_gen_imgs) {

    # graph mean check amount by income, marital status and kids counts
    ls_img$mv <- df_input_il_noninc_covar %>%
      ggplot(aes(x=ymin_group, y=log(v_alpha_il),
                 colour=checks,
                 shape=kids)) +
      facet_wrap( ~ marital + kids, ncol=5) +
      geom_point(size=1) +
      labs(title = paste0(slb_add_title,
                          'Marginal Value Gain Each Check, Conditional on: Marry+Kids+Income, ',
                          st_file_type),
           subtitle = stg_subtitle,
           x = 'Income Group',
           y = 'Log(Marginal Value Gain Per Check)',
           caption = stg_caption)
  }

  ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
  if ('checks_v' %in% ls_st_gen_imgs) {

    # graph mean check amount by income, marital status and kids counts
    ls_img$checks_v <- df_alloc_i_long_covar_v %>%
      filter(rho_val == ar_rho[1]) %>% ungroup() %>%
      mutate(ymin_group = as.numeric(ymin_group),
             kids = as.factor(kids),
             marital = as.factor(marital)) %>%
      ggplot(aes(x=ymin_group, y=checks,
                 colour=allocate_type,
                 shape=kids,
                 linetype=marital)) +
      facet_wrap( ~ marital + kids, ncol=5) +
      geom_point(size=3) +
      geom_line() +
      labs(title =
             paste0(slb_add_title,
                    'Optimize Expected 2020 Value, Conditional on: Marry+Kids+Income, ',
                    st_file_type),
           subtitle = stg_subtitle,
           x = 'Income Group',
           y = 'Number of Checks',
           caption = stg_caption)

  }

  ## ---- Image Return List
  ar_st_img_names <- names(ls_img)
  if (bl_save_img) {
    for (it_img_ctr in seq(1,length(ar_st_img_names))) {

      # Image Name
      st_img_name <- ar_st_img_names[it_img_ctr]
      st_img_name_full <- paste0(st_img_suffix, '_', st_img_name, '.png')

      if(st_img_name %in% ls_st_save_imgs) {
        # PNG and print
        png(paste0(spt_img_save, st_img_name_full),
            width = it_img_width,
            height = it_img_height, units=st_img_units,
            res = it_img_res, pointsize=it_img_pointsize)
        print(ls_img[it_img_ctr])
        dev.off()
      }

    }
  }

  # Complete and Return
  return(ls_img)

}
