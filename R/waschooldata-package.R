#' waschooldata: Fetch and Process Washington State School Data
#'
#' Downloads and processes school data from the Washington Office of
#' Superintendent of Public Instruction (OSPI). Provides functions for
#' fetching enrollment data from the Washington State Report Card and
#' transforming it into tidy format for analysis.
#'
#' @section Data Source:
#' Data is sourced from the Washington State Report Card via the data.wa.gov
#' open data portal (Socrata API). The Report Card provides comprehensive
#' enrollment data disaggregated by:
#' \itemize{
#'   \item Organization level (State, District, School)
#'   \item Grade level (PK through 12)
#'   \item Gender
#'   \item Race/Ethnicity
#'   \item Special populations (ELL, Low Income, Special Ed, etc.)
#' }
#'
#' @section Data Availability:
#' \itemize{
#'   \item Years: 2015 to present (approximately 10 years)
#'   \item Aggregation levels: State, District, School
#'   \item Update frequency: Annual (October enrollment count)
#' }
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{\link{fetch_enr}}: Download enrollment data for a single year
#'   \item \code{\link{fetch_enr_multi}}: Download enrollment data for multiple years
#'   \item \code{\link{tidy_enr}}: Transform wide data to long format
#'   \item \code{\link{get_available_years}}: List available data years
#' }
#'
#' @section Washington Identifiers:
#' \itemize{
#'   \item District Organization ID: Unique identifier for each district
#'   \item School Organization ID: Unique identifier for each school
#'   \item District Code: Short code for district identification
#'   \item School Code: Short code for school identification
#' }
#'
#' @docType package
#' @name waschooldata-package
#' @aliases waschooldata
"_PACKAGE"
