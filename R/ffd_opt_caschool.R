#' Test Dataset California Test Score Data
#'
#' 420 school level observations a cross-section from 1998-1999
#' distcod, district code
#' county, county
#' district, district
#' grspan, grade span of district
#' enrltot, total enrollment
#' teachers, number of teachers
#' calwpct, percent qualifying for CalWorks
#' mealpct, percent qualifying for reduced-price lunch
#' computer, number of computers
#' testscr, average test score (read.scr+math.scr)/2
#' compstu, computer per student
#' expnstu, expenditure per student
#' str, student teacher ratio
#' avginc, district average income
#' elpct, percent of English learners
#' readscr, average reading score
#' mathscr, average math score
#'
#' @docType data
#'
#' @usage data(df_opt_caschool)
#'
#' @format csv
#'
#' @keywords datasets
#'
#' @references Stock, James H. and Mark W. Watson (2003) Introduction to Econometrics, Addison-Wesley Educational Publishers, chapter 4–7.
#'
#' @source \url{https://fmwww.bc.edu/ec-p/data/stockwatson/caschool.des}
#'
#' @examples
#' data(df_opt_caschool)
#' head(df_opt_caschool, 10)
"df_opt_caschool"
