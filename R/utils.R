# ==============================================================================
# Utility Functions
# ==============================================================================

#' Pipe operator
#'
#' See \code{dplyr::\link[dplyr:reexports]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom dplyr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL


#' Convert to numeric, handling suppression markers
#'
#' OSPI uses various markers for suppressed data (*, <, >, N/A, etc.)
#' and may use commas in large numbers.
#'
#' @param x Vector to convert
#' @return Numeric vector with NA for non-numeric values
#' @keywords internal
safe_numeric <- function(x) {
  # Handle NULL or empty
  if (is.null(x) || length(x) == 0) {
    return(numeric(0))
  }

  # Convert to character if not already
  x <- as.character(x)

  # Remove commas and whitespace
  x <- gsub(",", "", x)
  x <- trimws(x)

  # Handle common suppression markers
  # OSPI uses "<" and ">" for small cell suppression
  x[x %in% c("*", ".", "-", "-1", "N/A", "NA", "", "null")] <- NA_character_
  x[grepl("^<", x)] <- NA_character_
  x[grepl("^>", x)] <- NA_character_

  suppressWarnings(as.numeric(x))
}


#' Convert school year string to end year
#'
#' Converts "2023-24" format to 2024 (end year)
#'
#' @param school_year Character string like "2023-24" or "2023-2024"
#' @return Integer end year
#' @keywords internal
parse_school_year <- function(school_year) {
  if (is.null(school_year) || is.na(school_year)) {
    return(NA_integer_)
  }

  # Handle "2023-24" format
  if (grepl("^\\d{4}-\\d{2}$", school_year)) {
    start_year <- as.integer(substr(school_year, 1, 4))
    return(start_year + 1L)
  }

  # Handle "2023-2024" format
  if (grepl("^\\d{4}-\\d{4}$", school_year)) {
    return(as.integer(substr(school_year, 6, 9)))
  }

  # Handle single year
  if (grepl("^\\d{4}$", school_year)) {
    return(as.integer(school_year))
  }

  NA_integer_
}


#' Format end year as school year string
#'
#' Converts 2024 to "2023-24" format
#'
#' @param end_year Integer end year (e.g., 2024)
#' @return Character string like "2023-24"
#' @keywords internal
format_school_year <- function(end_year) {
  start_year <- end_year - 1
  end_short <- substr(as.character(end_year), 3, 4)
  paste0(start_year, "-", end_short)
}


#' Get available years for Washington enrollment data
#'
#' Returns a vector of end years for which enrollment data is available
#' from the Washington State Report Card data portal.
#'
#' @return Integer vector of available end years
#' @export
#' @examples
#' \dontrun{
#' available <- get_available_years()
#' print(available)
#' }
get_available_years <- function() {

  # Washington State Report Card enrollment data is available on data.wa.gov
  # Confirmed dataset IDs are available for 2019-2025
  # Earlier years (2015-2018) may be available but dataset IDs need confirmation
  # See get_dataset_id() for the specific IDs
  2019:2025
}


#' Get dataset ID for a given school year
#'
#' Returns the Socrata dataset ID for the Report Card Enrollment data
#' for a given school year end.
#'
#' @param end_year School year end (e.g., 2024 for 2023-24 school year)
#' @return Character string with dataset ID, or NULL if not available
#' @keywords internal
get_dataset_id <- function(end_year) {
  # Dataset IDs from data.wa.gov for Report Card Enrollment
  # These are the 4-4 alphanumeric codes in the URLs
  # Verified from: https://data.wa.gov/browse?category=Education&q=report+card+enrollment
  dataset_ids <- list(
    "2025" = "2rwv-gs2e",
    "2024" = "q4ba-s3jc",
    "2023" = "dij7-mbxg",
    "2022" = "ymi4-syjv",
    "2021" = "nvpc-yr7b",
    "2020" = "gtd3-scga",
    "2019" = "u4gd-6wxx"
  )

  id <- dataset_ids[[as.character(end_year)]]

  if (is.null(id)) {
    return(NULL)
  }

  id
}


#' Clean and standardize column names
#'
#' Converts column names to snake_case and handles common variations.
#'
#' @param names Character vector of column names
#' @return Character vector of cleaned column names
#' @keywords internal
clean_column_names <- function(names) {
  # Convert to lowercase
  names <- tolower(names)

  # Replace spaces and special chars with underscores
  names <- gsub("[^a-z0-9]", "_", names)

  # Remove consecutive underscores
  names <- gsub("_+", "_", names)

  # Remove leading/trailing underscores
  names <- gsub("^_|_$", "", names)

  names
}
