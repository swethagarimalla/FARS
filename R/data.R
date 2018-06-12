#' US National Highway Traffic Safety Administration's Fatality Analysis Reporting System (2013-2015)
#'
#' @source US National Highway Traffic Safety Administration's Fatality Analysis Reporting System
#' Years 2013 to 2015 \url{https://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}
#' @format A data frame with a multitude of columns including year, month, and count of fatalities:
#' \describe{
#'  \item{YEAR}{A value between 2013 and 2015.}
#'  \item{MONTH}{A value between 1 and 12.}
#'  \item{FATALS}{An integer}
#'  \item{STATE}{A value between 1 and 50.}
#' }
#' @examples
#' \dontrun{
#'  FARS
#' }
"FARS"