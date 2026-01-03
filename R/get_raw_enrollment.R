# ==============================================================================
# Raw Enrollment Data Download Functions
# ==============================================================================
#
# This file contains functions for downloading raw enrollment data from OSPI.
# Data comes from the Washington State Report Card via data.wa.gov Socrata API.
#
# Data source: https://data.wa.gov (search "Report Card Enrollment" or "Student Enrollment")
# API documentation: https://dev.socrata.com/
#
# Format Eras:
# - Era 1 (2010-2024): data.wa.gov Socrata API with consistent column names
#   - 2015-2024: "Report Card Enrollment" datasets
#   - 2010-2014: "Student Enrollment" datasets (same schema)
# - Era 2 (1994-2001): Legacy format with different schema (not currently supported)
#   - Separate District/County/State enrollment datasets
#   - Different column names (af/am/bf/bm/etc. for race/gender)
#
# ==============================================================================

#' Download raw enrollment data from OSPI
#'
#' Downloads enrollment data from Washington State Report Card on data.wa.gov
#' using the Socrata Open Data API (SODA).
#'
#' @param end_year School year end (2024 = 2023-24 school year)
#' @return Data frame with raw enrollment data
#' @keywords internal
get_raw_enr <- function(end_year) {

  # Validate year
  available_years <- get_available_years()
  if (!end_year %in% available_years) {
    stop(paste0(
      "end_year must be between ", min(available_years), " and ", max(available_years),
      ". Year ", end_year, " is not available."
    ))
  }

  message(paste("Downloading OSPI enrollment data for", format_school_year(end_year), "..."))

  # Get dataset ID for this year
  dataset_id <- get_dataset_id(end_year)

  if (is.null(dataset_id)) {
    stop(paste("No dataset ID found for year", end_year))
  }

  # Download data using Socrata API
  df <- download_socrata_data(dataset_id, end_year)

  # Add end_year column if not present
  if (!"end_year" %in% names(df)) {
    df$end_year <- end_year
  }

  df
}


#' Download data from Socrata API
#'
#' Downloads data from data.wa.gov using the Socrata Open Data API.
#' Handles pagination for large datasets.
#'
#' @param dataset_id The 4-4 alphanumeric dataset identifier
#' @param end_year School year end (for error messages)
#' @return Data frame with all records
#' @keywords internal
download_socrata_data <- function(dataset_id, end_year) {

  # Build base URL for data.wa.gov
  base_url <- paste0("https://data.wa.gov/resource/", dataset_id, ".json")

  # Socrata API limits to 50000 records per request by default
  # We'll use pagination to get all records
  limit <- 50000
  offset <- 0
  all_data <- list()

  repeat {
    # Build URL with pagination
    url <- paste0(base_url, "?$limit=", limit, "&$offset=", offset)

    # Make request
    response <- tryCatch({
      httr::GET(
        url,
        httr::timeout(300),
        httr::add_headers(
          "Accept" = "application/json"
        )
      )
    }, error = function(e) {
      stop(paste("Failed to connect to data.wa.gov:", e$message))
    })

    # Check for HTTP errors
    if (httr::http_error(response)) {
      status <- httr::status_code(response)

      # Handle common errors
      if (status == 404) {
        stop(paste0(
          "Dataset not found for year ", end_year, " (dataset ID: ", dataset_id, ").\n",
          "The dataset may not be available or the ID may have changed.\n",
          "Check https://data.wa.gov for current datasets."
        ))
      } else if (status == 403) {
        stop("Access denied. The dataset may require authentication.")
      } else {
        stop(paste("HTTP error", status, "from data.wa.gov"))
      }
    }

    # Parse JSON response
    content <- httr::content(response, "text", encoding = "UTF-8")
    batch <- tryCatch({
      jsonlite::fromJSON(content, flatten = TRUE)
    }, error = function(e) {
      stop(paste("Failed to parse JSON response:", e$message))
    })

    # Check if we got data
    if (length(batch) == 0 || nrow(batch) == 0) {
      if (offset == 0) {
        stop(paste("No data returned for year", end_year))
      }
      break
    }

    all_data[[length(all_data) + 1]] <- batch
    message(paste("  Downloaded", nrow(batch), "records (offset:", offset, ")"))

    # Check if we got fewer than limit (means we're done)
    if (nrow(batch) < limit) {
      break
    }

    offset <- offset + limit
  }

  # Combine all batches
  df <- dplyr::bind_rows(all_data)

  message(paste("  Total records:", nrow(df)))

  df
}


#' Build Socrata API URL with filters
#'
#' Constructs a SODA API URL with optional filters.
#'
#' @param dataset_id The dataset identifier
#' @param filters Named list of column=value filters
#' @param select Character vector of columns to select (NULL for all)
#' @param limit Maximum records to return
#' @return Character URL
#' @keywords internal
build_socrata_url <- function(dataset_id, filters = NULL, select = NULL, limit = 50000) {

  base_url <- paste0("https://data.wa.gov/resource/", dataset_id, ".json")

  params <- list()

  # Add limit
  params[["$limit"]] <- limit

  # Add select clause
  if (!is.null(select)) {
    params[["$select"]] <- paste(select, collapse = ",")
  }

  # Add where clause for filters
  if (!is.null(filters) && length(filters) > 0) {
    where_clauses <- sapply(names(filters), function(col) {
      paste0(col, "='", filters[[col]], "'")
    })
    params[["$where"]] <- paste(where_clauses, collapse = " AND ")
  }

  # Build query string
  if (length(params) > 0) {
    query <- paste(names(params), params, sep = "=", collapse = "&")
    url <- paste0(base_url, "?", query)
  } else {
    url <- base_url
  }

  url
}


#' Get column mapping for Washington enrollment data
#'
#' Returns a list mapping raw column names to standardized names.
#' Washington Report Card data uses descriptive column names.
#'
#' @return Named list of column mappings
#' @keywords internal
get_wa_column_map <- function() {
  list(
    # Identifiers
    school_year = c("schoolyear", "school_year"),
    organization_level = c("organizationlevel", "organization_level"),
    county = c("county"),
    esd_name = c("esdname", "esd_name"),
    esd_id = c("esdorganizationid", "esd_organization_id"),
    district_code = c("districtcode", "district_code"),
    district_name = c("districtname", "district_name"),
    district_id = c("districtorganizationid", "district_organization_id"),
    school_code = c("schoolcode", "school_code"),
    school_name = c("schoolname", "school_name"),
    school_id = c("schoolorganizationid", "school_organization_id"),
    school_type = c("currentschooltype", "current_school_type"),
    grade_level = c("gradelevel", "grade_level"),

    # Total enrollment
    all_students = c("all_students", "allstudents"),

    # Gender
    female = c("female"),
    male = c("male"),
    gender_x = c("gender_x", "genderx"),

    # Race/Ethnicity
    native_american = c("american_indian_alaskan_native", "americanindianalaskannative"),
    asian = c("asian"),
    black = c("black_african_american", "blackafricanamerican"),
    hispanic = c("hispanic_latino_of_any_race", "hispaniclatino"),
    pacific_islander = c("native_hawaiian_other_pacific", "nativehawaiianotherpacific"),
    multiracial = c("two_or_more_races", "twoormoreraces"),
    white = c("white"),

    # Special populations
    ell = c("english_language_learners", "englishlanguagelearners"),
    non_ell = c("non_english_language_learners", "nonenglishlanguagelearners"),
    foster_care = c("foster_care", "fostercare"),
    non_foster_care = c("non_foster_care", "nonfostercare"),
    highly_capable = c("highly_capable", "highlycapable"),
    non_highly_capable = c("non_highly_capable", "nonhighlycapable"),
    homeless = c("homeless"),
    non_homeless = c("non_homeless", "nonhomeless"),
    low_income = c("low_income", "lowincome"),
    non_low_income = c("non_low_income", "nonlowincome"),
    migrant = c("migrant"),
    non_migrant = c("non_migrant", "nonmigrant"),
    military_parent = c("military_parent", "militaryparent"),
    non_military_parent = c("non_military_parent", "nonmilitaryparent"),
    mobile = c("mobile"),
    non_mobile = c("non_mobile", "nonmobile"),
    section_504 = c("section_504", "section504"),
    non_section_504 = c("non_section_504", "nonsection504"),
    special_ed = c("students_with_disabilities", "studentswithdisabilities"),
    non_special_ed = c("students_without_disabilities", "studentswithoutdisabilities"),

    # Metadata
    data_as_of = c("dataasof", "data_as_of")
  )
}
