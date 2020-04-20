rm(list = ls(all.names = TRUE))
# Load Libraries
library(dplyr)
library(tidyr)
library(stringr)
library(broom)
library(ggplot2)
library(REconTools)

library(PrjOptiAlloc)

library(knitr)
library(kableExtra)

# RMD Options
options(knitr.duplicate.label = "allow")
knitr::opts_chunk$set(fig.width=7, fig.height=4, fig.align="center")

# Output HTML or Latex
if (knitr::is_latex_output()) {
  options(knitr.table.format = "latex")
} else {
  options(knitr.table.format = "html")
}

# Table Output Options
kable_styling_fc = function(kable_input){
  kable_styling(kable_input,
    bootstrap_options = c("striped", "hover", "responsive"),
    latex_options = c("striped", "hold_position"),
    full_width = FALSE,
    fixed_thead = T,
    position = "center",
    font_size = NULL,
    row_label_position = "l")
}

# Table Output Options:
# 1. scale_down for TEX
# 2. box width: see R4Econ\style.css for body width, set width to bodywidth - 225
if (knitr::is_latex_output()) {
  kable_styling_fc_wide = function(kable_input){
    kable_styling(kable_input,
      bootstrap_options = c("striped", "hover", "responsive"),
      latex_options = c("striped", "scale_down", "hold_position"),
      full_width = FALSE,
      fixed_thead = T,
      position = "center",
      font_size = NULL,
      row_label_position = "l")
  }
} else {
  kable_styling_fc_wide = function(kable_input){
    kable_styling(kable_input,
      bootstrap_options = c("striped", "hover", "responsive"),
      latex_options = c("striped", "scale_down", "hold_position"),
      full_width = FALSE,
      fixed_thead = T,
      position = "center",
      font_size = NULL,
      row_label_position = "l") %>%
    scroll_box(width = "875px")
  }
}

# Get Current File Path
spt_file_current <- knitr::current_input(dir = TRUE)
print(paste0('spt_file_current:',spt_file_current))

sfc_prj='/PrjOptiAlloc'
sph_gitpages_root='https://fanwangecon.github.io/'
sph_github_root='https://github.com/FanWangEcon/'
sph_branch='/master'
sph_pdf='/htmlpdfr'
sph_html='/htmlpdfr'
sph_r='/htmlpdfr'

spt_root <- 'C:/Users/fan/PrjOptiAlloc/'
spn_prj_rmd <- gsub(spt_root, "", spt_file_current)
spt_rmd_path <- paste0('/',dirname(spn_prj_rmd))

st_fullpath_noname <- dirname(spt_file_current)
st_fullpath_nosufx <- sub('\\.Rmd$', '', spt_file_current)
st_file_wno_suffix <- sub('\\.Rmd$', '', basename(spt_file_current))
print(paste0('st_fullpath_noname:', st_fullpath_noname))
print(paste0('st_fullpath_nosufx:', st_fullpath_nosufx))
print(paste0('st_file_wno_suffix:', st_file_wno_suffix))

text_shared_preamble_one <- paste0("Back to **[Fan](https://fanwangecon.github.io/)**'s Optimal Allocation Homepage **[Table of Content](https://fanwangecon.github.io/PrjOptiAlloc/)**")
text_shared_preamble_two <- ""
text_shared_preamble_thr <- ""
