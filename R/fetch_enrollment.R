# ==============================================================================
# Enrollment Data Fetching Functions
# ==============================================================================
#
# This file contains functions for downloading enrollment data from the
# Washington Office of Superintendent of Public Instruction (OSPI).
#
# ==============================================================================

#' Fetch Washington enrollment data
#'
#' Downloads and processes enrollment data from the Washington State Report Card
#' via the data.wa.gov open data portal.
#'
#' @param end_year A school year. Year is the end of the academic year - eg 2023-24
#'   school year is year '2024'. Valid values are 2015-2025 (Socrata API).
#' @param tidy If TRUE (default), returns data in long (tidy) format with subgroup
#'   column. If FALSE, returns wide format.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#'   Set to FALSE to force re-download from OSPI.
#' @return Data frame with enrollment data. Wide format includes columns for
#'   district_id, campus_id, names, and enrollment counts by demographic/grade.
#'   Tidy format pivots these counts into subgroup and grade_level columns.
#' @export
#' @examples
#' \dontrun{
#' # Get 2024 enrollment data (2023-24 school year)
#' enr_2024 <- fetch_enr(2024)
#'
#' # Get wide format
#' enr_wide <- fetch_enr(2024, tidy = FALSE)
#'
#' # Force fresh download (ignore cache)
#' enr_fresh <- fetch_enr(2024, use_cache = FALSE)
#'
#' # Filter to specific district
#' seattle <- enr_2024 %>%
#'   dplyr::filter(district_name == "Seattle School District No. 1")
#' }
fetch_enr <- function(end_year, tidy = TRUE, use_cache = TRUE) {

  # Validate year
  available_years <- get_available_years()
  min_year <- min(available_years)
  max_year <- max(available_years)

  if (end_year < min_year || end_year > max_year) {
    stop(paste0(
      "end_year must be between ", min_year, " and ", max_year, ".\n",
      "Year ", end_year, " is not available.\n",
      "Use get_available_years() to see all available years."
    ))
  }

  # Determine cache type based on tidy parameter
  cache_type <- if (tidy) "tidy" else "wide"

  # Check cache first
  if (use_cache && cache_exists(end_year, cache_type)) {
    message(paste("Using cached data for", format_school_year(end_year)))
    return(read_cache(end_year, cache_type))
  }

  # Get raw data from OSPI
  raw <- get_raw_enr(end_year)

  # Process to standard schema
  processed <- process_enr(raw, end_year)

  # Add state aggregate if not present
  processed <- create_state_aggregate(processed, end_year)

  # Optionally tidy
  if (tidy) {
    processed <- tidy_enr(processed) %>%
      id_enr_aggs()
  }

  # Cache the result
  if (use_cache) {
    write_cache(processed, end_year, cache_type)
  }

  processed
}


#' Fetch enrollment data for multiple years
#'
#' Downloads and combines enrollment data for multiple school years.
#'
#' @param end_years Vector of school year ends (e.g., c(2022, 2023, 2024))
#' @param tidy If TRUE (default), returns data in long (tidy) format.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#' @return Combined data frame with enrollment data for all requested years
#' @export
#' @examples
#' \dontrun{
#' # Get 3 years of data
#' enr_multi <- fetch_enr_multi(2022:2024)
#'
#' # Track enrollment trends
#' enr_multi %>%
#'   dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
#'   dplyr::select(end_year, n_students)
#' }
fetch_enr_multi <- function(end_years, tidy = TRUE, use_cache = TRUE) {

  # Validate years
  available_years <- get_available_years()
  invalid_years <- end_years[!end_years %in% available_years]

  if (length(invalid_years) > 0) {
    stop(paste0(
      "Invalid years: ", paste(invalid_years, collapse = ", "), "\n",
      "Available years: ", min(available_years), "-", max(available_years), "\n",
      "Use get_available_years() to see all available years."
    ))
  }

  # Fetch each year
  results <- purrr::map(
    end_years,
    function(yr) {
      message(paste("Fetching", format_school_year(yr), "..."))
      fetch_enr(yr, tidy = tidy, use_cache = use_cache)
    }
  )

  # Combine
  dplyr::bind_rows(results)
}
