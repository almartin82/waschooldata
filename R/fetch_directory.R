# ==============================================================================
# School Directory Data Fetching Functions
# ==============================================================================
#
# This file contains functions for downloading school directory data from the
# Washington Office of Superintendent of Public Instruction (OSPI).
#
# Directory data combines two OSPI sources:
#
# 1. Report Card Enrollment datasets (data.wa.gov Socrata API):
#    - School names, codes, organization IDs
#    - District names, codes, organization IDs
#    - ESD names and IDs
#    - County
#    - School type codes
#    Provides school-level and district-level entity records.
#
# 2. EDS Education Directory (eds.ospi.k12.wa.us/directoryeds.aspx):
#    - District addresses
#    - Superintendent/administrator names
#    - Phone numbers and email addresses
#    Provides district-level contact information only.
#
# ==============================================================================

#' Fetch Washington school directory data
#'
#' Downloads and processes school and district directory data from the
#' Washington OSPI. School/district entity data comes from the Report Card
#' enrollment dataset on data.wa.gov. District contact information (address,
#' phone, email, administrator) comes from the OSPI Education Directory System.
#'
#' @param end_year School year end to use for entity data. Defaults to the most
#'   recent available year. Year is the end of the academic year, e.g. 2025 for
#'   the 2024-25 school year.
#' @param tidy If TRUE (default), returns data in a standardized format with
#'   consistent column names. If FALSE, returns with minimal processing.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#'   Set to FALSE to force re-download from OSPI.
#' @return A data frame with directory data. Key columns include:
#'   \describe{
#'     \item{entity_type}{"State", "District", or "School"}
#'     \item{state_district_id}{OSPI district organization ID}
#'     \item{district_code}{Short district code}
#'     \item{district_name}{District name}
#'     \item{state_school_id}{OSPI school organization ID (schools only)}
#'     \item{school_code}{Short school code (schools only)}
#'     \item{school_name}{School name (schools only)}
#'     \item{school_type}{School type code (P=Public, S=Special, etc.)}
#'     \item{county_name}{County}
#'     \item{esd_name}{Educational Service District name}
#'     \item{esd_id}{ESD organization ID}
#'     \item{address}{Street address (districts only, from EDS)}
#'     \item{city}{City (districts only)}
#'     \item{state}{State (always "WA")}
#'     \item{zip}{ZIP code (districts only, from EDS)}
#'     \item{administrator_name}{Superintendent name (districts only, from EDS)}
#'     \item{phone}{Phone number (districts only, from EDS)}
#'     \item{email}{Email address (districts only, from EDS)}
#'   }
#' @export
#' @examples
#' \dontrun{
#' # Get directory data
#' dir_data <- fetch_directory()
#'
#' # Get raw format
#' dir_raw <- fetch_directory(tidy = FALSE)
#'
#' # Force fresh download
#' dir_fresh <- fetch_directory(use_cache = FALSE)
#'
#' # Filter to schools only
#' library(dplyr)
#' schools <- dir_data |>
#'   filter(entity_type == "School")
#'
#' # Find all schools in Seattle
#' seattle <- dir_data |>
#'   filter(district_name == "Seattle School District No. 1",
#'          entity_type == "School")
#'
#' # Districts with contact info
#' districts <- dir_data |>
#'   filter(entity_type == "District", !is.na(phone))
#' }
fetch_directory <- function(end_year = NULL, tidy = TRUE, use_cache = TRUE) {

  # Default to most recent available year
  if (is.null(end_year)) {
    end_year <- max(get_available_years())
  }

  # Validate year
  available_years <- get_available_years()
  if (!end_year %in% available_years) {
    stop(paste0(
      "end_year must be between ", min(available_years), " and ",
      max(available_years), ". Year ", end_year, " is not available."
    ))
  }

  # Determine cache type
  cache_type <- if (tidy) "directory_tidy" else "directory_raw"

  # Check cache first
  if (use_cache && cache_exists_directory(end_year, cache_type)) {
    message(paste("Using cached directory data for", format_school_year(end_year)))
    return(read_cache_directory(end_year, cache_type))
  }

  # Get entity data from enrollment API (schools, districts, state)
  message(paste(
    "Downloading directory data from OSPI for",
    format_school_year(end_year), "..."
  ))
  entity_data <- get_directory_entities(end_year)

  # Get district contact info from EDS directory
  eds_data <- tryCatch({
    get_eds_directory()
  }, error = function(e) {
    warning(
      "Could not download EDS directory data: ", e$message,
      "\nDistrict contact info will not be available."
    )
    NULL
  })

  # Process and combine
  if (tidy) {
    result <- process_directory(entity_data, eds_data, end_year)
  } else {
    # Minimal processing for raw output
    result <- entity_data
    if (!is.null(eds_data)) {
      result <- merge_eds_data(result, eds_data)
    }
  }

  # Cache the result
  if (use_cache) {
    write_cache_directory(result, end_year, cache_type)
  }

  result
}


#' Clear school directory cache
#'
#' Removes cached school directory data files.
#'
#' @return Invisibly returns the number of files removed
#' @export
#' @examples
#' \dontrun{
#' clear_directory_cache()
#' }
clear_directory_cache <- function() {
  cache_dir <- get_cache_dir()

  if (!dir.exists(cache_dir)) {
    message("Cache directory does not exist")
    return(invisible(0))
  }

  files <- list.files(cache_dir, pattern = "^directory_", full.names = TRUE)

  if (length(files) > 0) {
    file.remove(files)
    message(paste("Removed", length(files), "cached directory file(s)"))
  } else {
    message("No cached directory files to remove")
  }

  invisible(length(files))
}


# ==============================================================================
# Internal: Data retrieval functions
# ==============================================================================

#' Get directory entities from enrollment API
#'
#' Extracts unique school and district entities from the enrollment data
#' on data.wa.gov. This provides the core directory information: names,
#' codes, types, counties, and ESD assignments.
#'
#' @param end_year School year end
#' @return Data frame with unique entity records
#' @keywords internal
get_directory_entities <- function(end_year) {

  dataset_id <- get_dataset_id(end_year)
  if (is.null(dataset_id)) {
    stop(paste("No dataset ID found for year", end_year))
  }

  base_url <- paste0("https://data.wa.gov/resource/", dataset_id, ".json")

  # Select only the columns we need for directory data
  # Use "All Grades" rows to get one record per entity
  select_cols <- paste(
    "organizationlevel",
    "county",
    "esdname",
    "esdorganizationid",
    "districtcode",
    "districtname",
    "districtorganizationid",
    "schoolcode",
    "schoolname",
    "schoolorganizationid",
    "currentschooltype",
    sep = ","
  )

  # Build URL with filters for "All Grades" to get one row per entity
  url <- paste0(
    base_url,
    "?$select=", select_cols,
    "&$where=", utils::URLencode("gradelevel='All Grades'", reserved = TRUE),
    "&$limit=50000"
  )

  response <- httr::GET(
    url,
    httr::timeout(120),
    httr::add_headers("Accept" = "application/json")
  )

  if (httr::http_error(response)) {
    stop(paste(
      "Failed to download directory data from data.wa.gov.",
      "HTTP status:", httr::status_code(response)
    ))
  }

  content <- httr::content(response, "text", encoding = "UTF-8")
  df <- jsonlite::fromJSON(content, flatten = TRUE)

  if (!is.data.frame(df) || nrow(df) == 0) {
    stop("No entity data returned from data.wa.gov")
  }

  # Deduplicate -- enrollment data may have multiple rows per entity
  # for different grade levels, but we filtered to "All Grades"
  df <- unique(df)

  message(paste("  Retrieved", nrow(df), "entity records"))

  df
}


#' Get district directory from EDS
#'
#' Downloads the OSPI Education Directory System page and parses the HTML
#' table to extract district contact information including addresses,
#' administrator names, phone numbers, and email addresses.
#'
#' @return Data frame with district contact information
#' @keywords internal
get_eds_directory <- function() {

  url <- "https://eds.ospi.k12.wa.us/directoryeds.aspx"

  message("  Downloading EDS district directory...")

  response <- httr::GET(
    url,
    httr::timeout(120),
    httr::user_agent("Mozilla/5.0 (compatible; waschooldata R package)")
  )

  if (httr::http_error(response)) {
    stop(paste("EDS directory returned HTTP", httr::status_code(response)))
  }

  html <- httr::content(response, "text", encoding = "UTF-8")

  # Parse the HTML table
  eds_df <- parse_eds_html(html)

  message(paste("  Retrieved", nrow(eds_df), "district records from EDS"))

  eds_df
}


#' Parse EDS directory HTML table
#'
#' Extracts district directory data from the EDS HTML page using
#' string processing. The page contains a single HTML table with
#' district-level information.
#'
#' @param html Raw HTML string from the EDS directory page
#' @return Data frame with parsed district data
#' @keywords internal
parse_eds_html <- function(html) {

  # Extract table rows from the HTML
  # The table uses <tr> tags with <td> cells
  # Each row has: ESD Name, LEA Code, District Name, Address1, Address2,
  #               State, Zipcode, Administrator Name, Phone, Email


  # Extract all table rows (data rows have class patterns from ASP.NET GridView)
  row_pattern <- "<tr[^>]*>\\s*((?:<td[^>]*>.*?</td>\\s*)+)</tr>"
  row_matches <- gregexpr(row_pattern, html, perl = TRUE, ignore.case = TRUE)
  rows <- regmatches(html, row_matches)[[1]]

  if (length(rows) == 0) {
    stop("Could not parse EDS directory HTML - no table rows found")
  }

  # Parse each row into cells
  cell_pattern <- "<td[^>]*>(.*?)</td>"
  parsed_rows <- lapply(rows, function(row) {
    cell_matches <- gregexpr(cell_pattern, row, perl = TRUE, ignore.case = TRUE)
    cells <- regmatches(row, cell_matches)[[1]]

    # Extract content from each cell
    cell_values <- gsub(cell_pattern, "\\1", cells, perl = TRUE, ignore.case = TRUE)

    # Clean HTML tags from cell values (e.g., <a href> in email)
    cell_values <- gsub("<[^>]+>", "", cell_values)

    # Decode HTML entities
    cell_values <- gsub("&amp;", "&", cell_values)
    cell_values <- gsub("&lt;", "<", cell_values)
    cell_values <- gsub("&gt;", ">", cell_values)
    cell_values <- gsub("&#39;", "'", cell_values)
    cell_values <- gsub("&quot;", '"', cell_values)
    cell_values <- gsub("&nbsp;", " ", cell_values)

    # Trim whitespace
    cell_values <- trimws(cell_values)

    cell_values
  })

  # Filter to data rows (should have exactly 10 columns)
  # Header rows and empty rows will have different column counts
  data_rows <- parsed_rows[sapply(parsed_rows, length) == 10]

  if (length(data_rows) == 0) {
    # Try with other common column counts
    col_counts <- table(sapply(parsed_rows, length))
    most_common <- as.integer(names(which.max(col_counts)))

    if (most_common > 5) {
      data_rows <- parsed_rows[sapply(parsed_rows, length) == most_common]
    }

    if (length(data_rows) == 0) {
      stop("Could not identify data rows in EDS directory HTML")
    }
  }

  # Skip the first row if it looks like a header
  first_row <- data_rows[[1]]
  if (any(grepl("ESD|LEA|Agency|Admin|Phone", first_row, ignore.case = TRUE))) {
    data_rows <- data_rows[-1]
  }

  # Build data frame
  # Column order: ESD Name, LEA Code, District Name, Address1, Address2,
  #               State, Zipcode, Administrator Name, Phone, Email
  n_cols <- length(data_rows[[1]])

  df <- data.frame(
    matrix(unlist(data_rows), ncol = n_cols, byrow = TRUE),
    stringsAsFactors = FALSE
  )

  # Assign column names based on expected EDS table structure
  if (ncol(df) == 10) {
    names(df) <- c(
      "eds_esd_name", "eds_lea_code", "eds_district_name",
      "eds_address1", "eds_address2",
      "eds_state", "eds_zip",
      "eds_administrator", "eds_phone", "eds_email"
    )
  } else {
    # Fallback naming
    names(df) <- paste0("col_", seq_len(ncol(df)))
    warning(paste("EDS table had", ncol(df), "columns instead of expected 10"))
  }

  # Clean up empty strings to NA
  df[] <- lapply(df, function(x) {
    x[x == "" | x == " "] <- NA_character_
    x
  })

  df
}


# ==============================================================================
# Internal: Data processing functions
# ==============================================================================

#' Process directory data into standardized schema
#'
#' Combines entity data from the enrollment API with district contact
#' information from the EDS directory into a clean, standardized format.
#'
#' @param entity_data Data frame from get_directory_entities()
#' @param eds_data Data frame from get_eds_directory(), or NULL
#' @param end_year School year end
#' @return Processed data frame with standard directory schema
#' @keywords internal
process_directory <- function(entity_data, eds_data, end_year) {

  # Standardize column names from Socrata
  names(entity_data) <- clean_column_names(names(entity_data))

  n_rows <- nrow(entity_data)
  result <- dplyr::tibble(.rows = n_rows)

  # End year
  result$end_year <- rep(end_year, n_rows)

  # Entity type
  org_col <- find_directory_col(entity_data, c(
    "organizationlevel", "organization_level"
  ))
  if (!is.null(org_col)) {
    result$entity_type <- dplyr::case_when(
      entity_data[[org_col]] == "State" ~ "State",
      entity_data[[org_col]] == "District" ~ "District",
      entity_data[[org_col]] == "School" ~ "School",
      TRUE ~ entity_data[[org_col]]
    )
  }

  # District identifiers
  dist_id_col <- find_directory_col(entity_data, c(
    "districtorganizationid", "district_organization_id"
  ))
  if (!is.null(dist_id_col)) {
    result$state_district_id <- as.character(entity_data[[dist_id_col]])
  }

  dist_code_col <- find_directory_col(entity_data, c(
    "districtcode", "district_code"
  ))
  if (!is.null(dist_code_col)) {
    result$district_code <- as.character(entity_data[[dist_code_col]])
  }

  dist_name_col <- find_directory_col(entity_data, c(
    "districtname", "district_name"
  ))
  if (!is.null(dist_name_col)) {
    result$district_name <- trimws(entity_data[[dist_name_col]])
  }

  # School identifiers (NA for non-school rows)
  school_id_col <- find_directory_col(entity_data, c(
    "schoolorganizationid", "school_organization_id"
  ))
  if (!is.null(school_id_col)) {
    result$state_school_id <- as.character(entity_data[[school_id_col]])
    result$state_school_id[result$entity_type != "School"] <- NA_character_
  }

  school_code_col <- find_directory_col(entity_data, c(
    "schoolcode", "school_code"
  ))
  if (!is.null(school_code_col)) {
    result$school_code <- as.character(entity_data[[school_code_col]])
    result$school_code[result$entity_type != "School"] <- NA_character_
  }

  school_name_col <- find_directory_col(entity_data, c(
    "schoolname", "school_name"
  ))
  if (!is.null(school_name_col)) {
    result$school_name <- trimws(entity_data[[school_name_col]])
    result$school_name[result$entity_type != "School"] <- NA_character_
  }

  # School type
  type_col <- find_directory_col(entity_data, c(
    "currentschooltype", "current_school_type"
  ))
  if (!is.null(type_col)) {
    result$school_type <- entity_data[[type_col]]
  }

  # County
  county_col <- find_directory_col(entity_data, c("county"))
  if (!is.null(county_col)) {
    result$county_name <- trimws(entity_data[[county_col]])
  }

  # ESD info
  esd_name_col <- find_directory_col(entity_data, c(
    "esdname", "esd_name"
  ))
  if (!is.null(esd_name_col)) {
    result$esd_name <- trimws(entity_data[[esd_name_col]])
  }

  esd_id_col <- find_directory_col(entity_data, c(
    "esdorganizationid", "esd_organization_id"
  ))
  if (!is.null(esd_id_col)) {
    result$esd_id <- as.character(entity_data[[esd_id_col]])
  }

  # State is always WA
  result$state <- "WA"

  # Merge EDS contact data for districts
  if (!is.null(eds_data) && nrow(eds_data) > 0 &&
      "eds_district_name" %in% names(eds_data)) {
    result <- merge_eds_contact_tidy(result, eds_data)
  } else {
    # Add empty contact columns
    result$address <- NA_character_
    result$city <- NA_character_
    result$zip <- NA_character_
    result$administrator_name <- NA_character_
    result$phone <- NA_character_
    result$email <- NA_character_
  }

  # Reorder columns
  preferred_order <- c(
    "end_year", "entity_type",
    "state_district_id", "district_code", "district_name",
    "state_school_id", "school_code", "school_name",
    "school_type", "county_name", "esd_name", "esd_id",
    "address", "city", "state", "zip",
    "administrator_name", "phone", "email"
  )

  existing_cols <- preferred_order[preferred_order %in% names(result)]
  other_cols <- setdiff(names(result), preferred_order)
  result <- result[, c(existing_cols, other_cols)]

  result
}


#' Merge EDS contact data into tidy directory
#'
#' Joins district contact information from the EDS directory into the
#' processed directory data. Matching is done by district name since
#' the EDS page does not include OSPI organization IDs.
#'
#' @param result Processed directory data frame
#' @param eds_data EDS directory data frame
#' @return Updated data frame with contact columns
#' @keywords internal
merge_eds_contact_tidy <- function(result, eds_data) {

  # Prepare EDS data for joining
  eds_clean <- data.frame(
    eds_join_name = normalize_district_name(eds_data$eds_district_name),
    address = ifelse(
      is.na(eds_data$eds_address2) | eds_data$eds_address2 == "",
      eds_data$eds_address1,
      paste(eds_data$eds_address1, eds_data$eds_address2)
    ),
    zip = eds_data$eds_zip,
    administrator_name = eds_data$eds_administrator,
    phone = eds_data$eds_phone,
    email = eds_data$eds_email,
    stringsAsFactors = FALSE
  )

  # Extract city from address (WA addresses often have city in address)
  # EDS data has state column but city is embedded in address lines
  eds_clean$city <- NA_character_

  # Prepare result for joining
  result$eds_join_name <- normalize_district_name(result$district_name)

  # Left join on normalized district name
  result <- dplyr::left_join(
    result,
    eds_clean,
    by = "eds_join_name",
    relationship = "many-to-one"
  )

  # Only keep contact info for district rows
  non_district <- result$entity_type != "District"
  result$address[non_district] <- NA_character_
  result$city[non_district] <- NA_character_
  result$zip[non_district] <- NA_character_
  result$administrator_name[non_district] <- NA_character_
  result$phone[non_district] <- NA_character_
  result$email[non_district] <- NA_character_

  # Remove join key
  result$eds_join_name <- NULL

  result
}


#' Merge EDS data into raw directory output
#'
#' Simple merge for raw (tidy=FALSE) output.
#'
#' @param entity_data Raw entity data
#' @param eds_data EDS directory data
#' @return Merged data frame
#' @keywords internal
merge_eds_data <- function(entity_data, eds_data) {

  # Standardize names
  names(entity_data) <- clean_column_names(names(entity_data))

  dist_name_col <- NULL
  for (col in c("districtname", "district_name")) {
    if (col %in% names(entity_data)) {
      dist_name_col <- col
      break
    }
  }

  if (is.null(dist_name_col) || !"eds_district_name" %in% names(eds_data)) {
    return(entity_data)
  }

  # Add join key
  entity_data$eds_join_name <- normalize_district_name(entity_data[[dist_name_col]])
  eds_data$eds_join_name <- normalize_district_name(eds_data$eds_district_name)

  # Left join
  result <- dplyr::left_join(
    entity_data,
    eds_data,
    by = "eds_join_name",
    relationship = "many-to-one"
  )

  result$eds_join_name <- NULL

  result
}


#' Normalize district name for matching
#'
#' Normalizes district name strings to improve matching between the
#' enrollment API and EDS directory, which may format names differently.
#'
#' @param name Character vector of district names
#' @return Character vector of normalized names
#' @keywords internal
normalize_district_name <- function(name) {
  if (is.null(name)) return(NULL)

  result <- tolower(trimws(name))

  # Remove common suffixes/variations
  result <- gsub("\\s+school district\\s*$", " sd", result)
  result <- gsub("\\s+school dist\\s*$", " sd", result)
  result <- gsub("\\s+sch dist\\s*$", " sd", result)

  # Normalize "No." and "#" to standardize numbering
  result <- gsub("\\s+no\\.?\\s*", " no ", result)
  result <- gsub("\\s+#\\s*", " no ", result)

  # Remove extra whitespace
  result <- gsub("\\s+", " ", result)
  result <- trimws(result)

  result
}


#' Find a column in directory data
#'
#' Helper function to find a column by checking multiple possible names.
#'
#' @param df Data frame to search
#' @param candidates Character vector of possible column names
#' @return Column name found, or NULL
#' @keywords internal
find_directory_col <- function(df, candidates) {
  for (candidate in candidates) {
    clean_candidate <- clean_column_names(candidate)
    matched <- grep(
      paste0("^", clean_candidate, "$"),
      names(df),
      value = TRUE
    )
    if (length(matched) > 0) return(matched[1])
  }
  NULL
}


# ==============================================================================
# Directory-specific cache functions
# ==============================================================================

#' Build cache file path for directory data
#'
#' @param end_year School year end
#' @param cache_type Type of cache ("directory_tidy" or "directory_raw")
#' @return File path string
#' @keywords internal
build_cache_path_directory <- function(end_year, cache_type) {
  cache_dir <- get_cache_dir()
  file.path(cache_dir, paste0(cache_type, "_", end_year, ".rds"))
}


#' Check if cached directory data exists
#'
#' @param end_year School year end
#' @param cache_type Type of cache
#' @param max_age Maximum age in days (default 30)
#' @return Logical indicating if valid cache exists
#' @keywords internal
cache_exists_directory <- function(end_year, cache_type, max_age = 30) {
  cache_path <- build_cache_path_directory(end_year, cache_type)

  if (!file.exists(cache_path)) {
    return(FALSE)
  }

  file_info <- file.info(cache_path)
  age_days <- as.numeric(difftime(Sys.time(), file_info$mtime, units = "days"))

  age_days <= max_age
}


#' Read directory data from cache
#'
#' @param end_year School year end
#' @param cache_type Type of cache
#' @return Cached data frame
#' @keywords internal
read_cache_directory <- function(end_year, cache_type) {
  cache_path <- build_cache_path_directory(end_year, cache_type)
  readRDS(cache_path)
}


#' Write directory data to cache
#'
#' @param data Data frame to cache
#' @param end_year School year end
#' @param cache_type Type of cache
#' @return Invisibly returns the cache path
#' @keywords internal
write_cache_directory <- function(data, end_year, cache_type) {
  cache_path <- build_cache_path_directory(end_year, cache_type)
  cache_dir <- dirname(cache_path)

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  saveRDS(data, cache_path)
  invisible(cache_path)
}
