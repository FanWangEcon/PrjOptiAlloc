#' Test Dataset North Carolina Crimes
#'
#' A panel of 90 observations from 1981 to 1987
#' county, county identifier
#' year, year from 1981 to 1987
#' crmrte, crimes committed per person
#' prbarr, 'probability' of arrest
#' prbconv, 'probability' of conviction
#' prbpris, 'probability' of prison sentence
#' avgsen, average sentence, days
#' polpc, police per capita
#' density, people per square mile
#' taxpc, tax revenue per capita
#' region, one of 'other', 'west' or 'central'
#' smsa, 'yes' or 'no' if in SMSA
#' pctmin, percentage minority in 1980
#' wcon, weekly wage in construction
#' wtuc, weekly wage in trns, util, commun
#' wtrd, weekly wage in whole sales and retail trade
#' wfir, weekly wage in finance, insurance and real estate
#' wser, weekly wage in service industry
#' wmfg, weekly wage in manufacturing
#' wfed, weekly wage of federal employees
#' wsta, weekly wage of state employees
#' wloc, weekly wage of local governments employees
#' mix, offence mix: face-to-face/other
#' pctymle, percentage of young males
#'
#' @docType data
#'
#' @usage data(df_opt_nccrimes)
#'
#' @format csv
#'
#' @keywords datasets
#'
#' @references Cornwell, C. and W.N. Trumbull (1994) “Estimating the economic model of crime with panel data”, Review of Economics and Statistics, 76, 360–366.
#'
#' @source \url{https://www.jstor.org/stable/2109893?}
#'
#' @examples
#' data(df_opt_nccrimes)
#' head(df_opt_nccrimes, 10)
"df_opt_nccrimes"
