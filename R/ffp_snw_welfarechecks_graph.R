ffp_snw_graph_feasible <- function(ar_rho, df_input_il_noninc_covar,
                                   df_alloc_i_long_covar_c, df_alloc_i_long_covar_v,
                                   ls_st_gen_imgs = c('mass', 'mc', 'checks_c', 'mv', 'mlogc', 'checks_v', 'checks_cjv'),
                                   ls_st_save_imgs = c('mass', 'mc', 'checks_c', 'mv', 'mlogc', 'checks_v', 'checks_cjv'),
                                   st_file_type = '',
                                   slb_add_title = '',
                                   stg_subtitle='', stg_caption='',
                                   bl_no_title_caption = TRUE,
                                   bl_optimal_with_age_groups = FALSE,
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
  marry_levels <- c(Single = "0", Married = "1")
  kids_levels <- c("no children" = "0", "one child" = "1",
                   "two children" = "2", "three children" = "3",
                   "four children" = "4")

  ## Select, as factor, and recode
  df_input_il_noninc_covar <- df_input_il_noninc_covar %>%
    mutate(ymin_group = as.numeric(ymin_group),
           kids = as.factor(kids),
           marital = as.factor(marital)) %>%
    mutate(kids = fct_recode(kids, !!!kids_levels),
           marital = fct_recode(marital, !!!marry_levels))

  if (bl_optimal_with_age_groups) {
    df_alloc_i_long_covar_c <- df_alloc_i_long_covar_c %>% filter(allocate_type != 'Actual')
    df_alloc_i_long_covar_v <- df_alloc_i_long_covar_v %>% filter(allocate_type != 'Actual')
    df_alloc_i_long_covar_c <- df_alloc_i_long_covar_c %>% filter(allocate_type != 'Actual 1st')
    df_alloc_i_long_covar_v <- df_alloc_i_long_covar_v %>% filter(allocate_type != 'Actual 1st')
  }

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

  ## ---- Common X and Y Labels etc
  x.labels <- c('0', '50K', '100K', '150K', '200K')
  x.breaks <- c(1,
                round((50000 - 6514)/2505),
                round((100000 - 6514)/2505),
                round((150000 - 6514)/2505),
                round((200000 - 6514)/2505))

  ## ---- Image Return List
  ls_img <- vector(mode = "list", length = 0)
  bl_share_plot <- TRUE

  ## ---- Titling COnditioning Text
  if (bl_optimal_with_age_groups) {
    stg_conditions <- "Conditional on Kids/Marry/Income,"
  } else {
    stg_conditions <- "Conditional on Age-Groups/Kids/Marry/income,"
  }

  ## Loop over images to graph
  for (it_img_gen_ctr in seq(1, length(ls_st_gen_imgs))) {

    # Standard X and Y
    stg_x_title_hhinc <- 'Household income (thousands of 2012 USD)'
    stg_y_title_checks <- 'Amount of welfare checks (2012 USD)'

    # Current Image Name
    st_img_name <- ls_st_gen_imgs[it_img_gen_ctr]

    ## ------ G1: Mass
    if (st_img_name == 'mass') {
      stg_title <- paste0(slb_add_title, 'Mass At Each, ', stg_conditions, ' ', st_file_type)
      # graph mean check amount by income, marital status and kids counts
      plt_cur <- df_alloc_i_long_covar_c %>%
        filter(rho_val == ar_rho[1]) %>% ungroup() %>%
        # filter(marital == 1) %>%
        mutate(ymin_group = as.numeric(ymin_group),
               kids = as.factor(kids),
               marital = as.factor(marital)) %>%
        ggplot(aes(x=ymin_group, y=mass, colour=age_group))
      stg_y_title_checks <- 'Probability mass for this group'
    }

    ## ------ G2: MC
    if (st_img_name == 'mc') {
      stg_title <- paste0(slb_add_title, 'Marginal Consumption Gain Each Check, ', stg_conditions, st_file_type)
      # graph mean check amount by income, marital status and kids counts
      if (bl_optimal_with_age_groups) {
        plt_cur <- df_input_il_noninc_covar %>% filter(checks != 0) %>%
          ggplot(aes(x=ymin_group, y=log(c_alpha_il), colour=age_group))
      } else {
        plt_cur <- df_input_il_noninc_covar %>% filter(checks != 0) %>%
          ggplot(aes(x=ymin_group, y=log(c_alpha_il), colour=checks))
      }
      stg_y_title_checks <- 'Log((expected 2020 C with X checks) - (expected 2020 C with X-1 checks))'
    }

    ## ------ G3: MV
    if (st_img_name == 'mv') {
      stg_title <- paste0(slb_add_title, 'Marginal Value Gain Each Check, ', stg_conditions, st_file_type)
      # graph mean check amount by income, marital status and kids counts
      if (bl_optimal_with_age_groups) {
        plt_cur <- df_input_il_noninc_covar %>% filter(checks != 0) %>%
          ggplot(aes(x=ymin_group, y=log(v_alpha_il), colour=age_group))
      } else {
        plt_cur <- df_input_il_noninc_covar %>% filter(checks != 0) %>%
          ggplot(aes(x=ymin_group, y=log(v_alpha_il), colour=checks))
      }
      stg_y_title_checks <- 'Log((expected lifetime V with X checks) - (expected lifetime V with X-1 checks))'
    }

    ## ------ G4: mlogc
    if (st_img_name == 'mlogc') {
      stg_title <- paste0(slb_add_title, 'Log(C(checks))-log(C(checks-1)),', stg_conditions, st_file_type)
      # 1. Generate zero check c
      df_input_il_noninc_covar_difflogc <-
        rbind(df_input_il_noninc_covar %>%
                filter(checks == 1) %>%
                mutate(checks = 0) %>%
                mutate(c_A_il_log = log(c_A_il)),
              df_input_il_noninc_covar %>%
                mutate(c_A_il_log = log(c_A_il + c_alpha_il))) %>%
        arrange(id_i, checks) %>%
        group_by(id_i) %>%
        mutate(c_A_il_log_diff = c_A_il_log - lag(c_A_il_log))
      # 2. Graph Results
      if (bl_optimal_with_age_groups) {
        plt_cur <- df_input_il_noninc_covar_difflogc %>%
          filter(checks != 0) %>%
          ggplot(aes(x=ymin_group, y=c_A_il_log_diff, colour=age_group))
      } else {
        plt_cur <- df_input_il_noninc_covar_difflogc %>%
          filter(checks != 0) %>%
          ggplot(aes(x=ymin_group, y=c_A_il_log_diff, colour=checks))
      }
      stg_y_title_checks <- 'Log(expected 2020 C with X checks) - Log(expected 2020 C with X-1 checks)'
    }

    ## ------ G5: Checks_c
    if (st_img_name == 'checks_c') {
      stg_title <- paste0(slb_add_title, 'Optimize Expected 2020 Consumption, ', stg_conditions , st_file_type)
      # graph mean check amount by income, marital status and kids counts
      if (bl_optimal_with_age_groups) {
        plt_cur <- df_alloc_i_long_covar_c %>%
          filter(rho_val == ar_rho[1]) %>% ungroup() %>%
          mutate(ymin_group = as.numeric(ymin_group),
                 kids = as.factor(kids),
                 marital = as.factor(marital)) %>%
          ggplot(aes(x=ymin_group, y=checks*100,
                     colour=age_group,
                     shape=age_group))
      } else {
        plt_cur <- df_alloc_i_long_covar_c %>%
          filter(rho_val == ar_rho[1]) %>% ungroup() %>%
          mutate(ymin_group = as.numeric(ymin_group),
                 kids = as.factor(kids),
                 marital = as.factor(marital)) %>%
          ggplot(aes(x=ymin_group, y=checks*100,
                     colour=allocate_type,
                     shape=allocate_type))
      }
    }

    ## ------ G6: Checks_v
    if (st_img_name == 'checks_v') {
      stg_title <- paste0(slb_add_title, 'Optimize Expected 2020 Value, ', stg_conditions, st_file_type)
      # graph mean check amount by income, marital status and kids counts
      if (bl_optimal_with_age_groups) {
        plt_cur <- df_alloc_i_long_covar_v %>%
          filter(rho_val == ar_rho[1]) %>% ungroup() %>%
          mutate(ymin_group = as.numeric(ymin_group),
                 kids = as.factor(kids),
                 marital = as.factor(marital)) %>%
          ggplot(aes(x=ymin_group, y=checks*100,
                     colour=age_group,
                     shape=age_group))
      } else {
        plt_cur <- df_alloc_i_long_covar_v %>%
          filter(rho_val == ar_rho[1]) %>% ungroup() %>%
          mutate(ymin_group = as.numeric(ymin_group),
                 kids = as.factor(kids),
                 marital = as.factor(marital)) %>%
          ggplot(aes(x=ymin_group, y=checks*100,
                     colour=allocate_type,
                     shape=allocate_type))
      }
    }

    ## ------ G7: Checks_v vs c
    if (st_img_name == 'checks_cjv') {
      stg_title <- paste0(slb_add_title, '2020 Value and 2020 Consumption, ', stg_conditions, st_file_type)
      # Combine frames
      df_alloc_i_long_covar_cjv <- rbind(
          df_alloc_i_long_covar_v %>%
          filter(rho_val == ar_rho[1] & allocate_type == 'Optimal') %>%
          mutate(allocate_type = "1. V-allocation"),
          df_alloc_i_long_covar_c %>%
          filter(rho_val == ar_rho[1] & allocate_type == 'Optimal') %>%
          mutate(allocate_type = "2. C-allocation"))
      # graph mean check amount by income, marital status and kids counts
      plt_cur <- df_alloc_i_long_covar_cjv %>% ungroup() %>%
        mutate(ymin_group = as.numeric(ymin_group),
               kids = as.factor(kids),
               marital = as.factor(marital)) %>%
        ggplot(aes(x=ymin_group, y=checks*100,
                   colour=allocate_type,
                   shape=allocate_type))
    }

    ## ------ H Plot Options
    if (bl_share_plot) {

      # Main plot
      plt_cur <- plt_cur +
        facet_wrap( ~ marital + kids, ncol=5, labeller = label_wrap_gen(multi_line=FALSE))

      # Add geom_point differing sizes
      ls_st_geompoint_size1 = c('mc', 'mv', 'mlogc')
      if (st_img_name %in% ls_st_geompoint_size1) {
        plt_cur <- plt_cur + geom_point(size=1)
      } else {
        plt_cur <- plt_cur + geom_point(size=3)
      }

      # Add geom_line
      ls_st_geomline = c('mass', 'checks_c', 'checks_v', 'checks_cjv')
      # ls_st_geomline = c('mass', 'checks_c', 'checks_v', 'mc', 'mv', 'mlogc')
      if (st_img_name %in% ls_st_geomline) {
        plt_cur <- plt_cur + geom_line()
      }

      # Add check legend
      ls_st_checks = c('mc', 'mv', 'mlogc')
      # Add to PLo
      if (st_img_name %in% ls_st_checks) {
        plt_cur <- plt_cur +
          guides(colour = guide_colorbar(
            title = "checks",
            frame.colour = "black",
            barwidth = 1.5,
            barheight = 15)
          ) +
          theme(
            legend.position = c(0.14, 0.9),
            legend.background = element_rect(fill = "white", colour = "black", linetype='solid')) +
          scale_x_continuous(labels = x.labels, breaks = x.breaks)
      } else {
        plt_cur <- plt_cur +
          theme(
            legend.title = element_blank(),
            legend.position = c(0.14, 0.9),
            legend.background = element_rect(fill = "white", colour = "black", linetype='solid')) +
          scale_x_continuous(labels = x.labels, breaks = x.breaks)
      }
      # legend.text = element_text(margin = margin(t = 10))) +

      # title and notes
      if (bl_no_title_caption) {
        plt_cur <- plt_cur + labs(x = stg_x_title_hhinc, y = stg_y_title_checks)
      } else {
        plt_cur <- plt_cur +
          labs(title = stg_title, subtitle = stg_subtitle,
               x = stg_x_title_hhinc, y = stg_y_title_checks,
               caption = stg_caption)
      }

    }

    ls_img[[st_img_name]] <- plt_cur

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
