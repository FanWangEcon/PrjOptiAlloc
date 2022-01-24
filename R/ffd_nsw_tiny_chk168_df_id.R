#' NSW Test Household ID file
#'
#' Test dataset simulated for NSW
#' id_i: individual ID
#' martial: binary marital status (0 = unmarried, 1 = married)
#' kids: the number of kids in household
#' age_group: age-group households belong to, aggregated to a single group between 17 and 64.
#' mass: mass of population in this household given income, marital, and kids status.
#' hhsize: number of household members given kids count and marital status
#'
#' @docType data
#'
#' @usage data(df_nsw_tiny_chk168_df_id)
#'
#' @format csv
#'
#' @keywords datasets
#'
#' @references Vegard M. Nygaard, Bent E. Sørensen, and Fan Wang (2021), Optimal Allocations to Heterogeneous Agents with an Application to Stimulus Checks
#'
#' @source \url{https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3839890}
#'
#' @examples
#' data(df_nsw_tiny_chk168_df_id)
#' head(df_nsw_tiny_chk168_df_id, 10)
"df_nsw_tiny_chk168_df_id"
