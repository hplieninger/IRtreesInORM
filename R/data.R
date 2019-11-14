#' Names of the 11 countries included in the multiple-group analyses.
#'
#' A character vector containing the names.
#'
#' @usage data("ctrs")
"ctrs"

#' RowIDs of 500 randomly selected respondents per country.
#'
#' A random subsample of at most 500 respondents (with complete cases on the variables v45--v47) was selected in each country.
#' The vector \code{id_subset} contains the rowIDs of these respondents pertaining to \code{issp15$rowid}.
#'
#' @usage data("id_subset")
#' @examples
#' subset(issp15, subset = rowid %in% id_subset)
"id_subset"

#' The data from ISSP 2015.
#'
#' A data set containing the data from 51,668 persons (from 37 countries) on 443 variables.
#'
#' @format A tibble/data frame with 51,668 rows and 443 columns:
#' \describe{
#'   \item{rowid}{Row number in the SPSS data file}
#'   \item{country}{Country of respondent}
#'   \item{v45}{I am willing to work harder than I have to in order to help the firm or organization I work for succeed.}
#'   \item{v46}{I am proud to be working for my firm or organization.}
#'   \item{v47}{I would turn down another job that offered quite a bit more pay in order to stay with this organization.}
#' }
#' @references ISSP Research Group. (2017). International Social Survey Programme: Work Orientations IV - ISSP 2015 \[Data file Version 2.1.0\].
#' @source \url{http://dx.doi.org/10.4232/1.12848}
#' @usage data("issp15")
"issp15"

#' The generated data and parameters of the simulation study.
#'
#' A list containing the data `sim_data$dat` and the data-generating parameters.
#'
#' @usage data("sim_data")
"sim_data"

#' The Mplus output for the three models fitted in the simulation study.
#'
#' A tibble, where each of three rows contains the Mplus output in the column `output`.
#'
#' @usage data("sim_res")
"sim_res"
