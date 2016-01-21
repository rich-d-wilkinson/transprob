#'
#' Financial ratings of 1505 financial assets
#'
#' A dataset containing 4315 ratings of 1505 financial assets. 
#'
#' @format A data frame with 4315 rows and 3 variables:
#' \describe{
#'   \item{ID}{Unique asset ID}
#'   \item{Date}{Inspection date in format \%d-\%b-\%Y}
#'   \item{Rating}{Financial rating: a factor with value in 'A', 'AA', 'AAA', 'B','BB','BBB','CCC','D'}
#' }
#' @source MATLAB; load Data_TransProb
"FinancialRating"


#'
#' Embankment data
#' 
#' A dataset containing 178515 ratings of 97076 unique embankment assets
#'
#' @format A data frame with 178515 rows and 3 columns:
#' \describe{
#'   \item{ID}{Unique asset ID}
#'   \item{Date}{Inspection date in format \%d-\%b-\%Y}
#'   \item{EHC}{Earthworks hazard category: a factor with value in 'A', 'B','C','D', 'E'}
#' }
"Embankment"