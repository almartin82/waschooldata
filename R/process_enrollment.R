# ==============================================================================
# Enrollment Data Processing Functions
# ==============================================================================
#
# This file contains functions for processing raw OSPI enrollment data into a
# clean, standardized format.
#
# ==============================================================================

#' Process raw OSPI enrollment data
#'
#' Transforms raw Report Card data into a standardized schema with
#' state, district, and school level aggregations.
#'
#' @param raw_data Data frame from get_raw_enr
#' @param end_year School year end
#' @return Processed data frame with standardized columns
#' @keywords internal
process_enr <- function(raw_data, end_year) {

  # Standardize column names
  names(raw_data) <- clean_column_names(names(raw_data))

  # Get column mapping
  col_map <- get_wa_column_map()

  # Find matching columns
  find_col <- function(patterns) {
    for (pattern in patterns) {
      pattern_clean <- clean_column_names(pattern)
      matched <- grep(paste0("^", pattern_clean, "$"), names(raw_data), value = TRUE)
      if (length(matched) > 0) return(matched[1])
    }
    NULL
  }

  # Build result dataframe
  n_rows <- nrow(raw_data)

  result <- data.frame(
    end_year = rep(end_year, n_rows),
    stringsAsFactors = FALSE
  )

  # Organization level determines type (State, District, School)
  org_col <- find_col(col_map$organization_level)
  if (!is.null(org_col)) {
    raw_org <- raw_data[[org_col]]
    result$type <- dplyr::case_when(
      raw_org == "State" ~ "State",
      raw_org == "District" ~ "District",
      raw_org == "School" ~ "Campus",
      TRUE ~ raw_org
    )
  } else {
    result$type <- NA_character_
  }

  # District info
  district_id_col <- find_col(col_map$district_id)
  if (!is.null(district_id_col)) {
    result$district_id <- as.character(raw_data[[district_id_col]])
  }

  district_code_col <- find_col(col_map$district_code)
  if (!is.null(district_code_col)) {
    result$district_code <- as.character(raw_data[[district_code_col]])
  }

  district_name_col <- find_col(col_map$district_name)
  if (!is.null(district_name_col)) {
    result$district_name <- trimws(raw_data[[district_name_col]])
  }

  # School info (campus)
  school_id_col <- find_col(col_map$school_id)
  if (!is.null(school_id_col)) {
    result$campus_id <- as.character(raw_data[[school_id_col]])
    # Set to NA for non-school rows
    result$campus_id[result$type != "Campus"] <- NA_character_
  }

  school_code_col <- find_col(col_map$school_code)
  if (!is.null(school_code_col)) {
    result$school_code <- as.character(raw_data[[school_code_col]])
    result$school_code[result$type != "Campus"] <- NA_character_
  }

  school_name_col <- find_col(col_map$school_name)
  if (!is.null(school_name_col)) {
    result$campus_name <- trimws(raw_data[[school_name_col]])
    result$campus_name[result$type != "Campus"] <- NA_character_
  }

  # School type
  school_type_col <- find_col(col_map$school_type)
  if (!is.null(school_type_col)) {
    result$school_type <- raw_data[[school_type_col]]
  }

  # ESD info
  esd_id_col <- find_col(col_map$esd_id)
  if (!is.null(esd_id_col)) {
    result$esd_id <- as.character(raw_data[[esd_id_col]])
  }

  esd_name_col <- find_col(col_map$esd_name)
  if (!is.null(esd_name_col)) {
    result$esd_name <- trimws(raw_data[[esd_name_col]])
  }

  # County
  county_col <- find_col(col_map$county)
  if (!is.null(county_col)) {
    result$county <- trimws(raw_data[[county_col]])
  }

  # Grade level (for grade-specific rows)
  grade_col <- find_col(col_map$grade_level)
  if (!is.null(grade_col)) {
    result$grade_level_raw <- raw_data[[grade_col]]
    # Standardize grade levels
    result$grade_level <- standardize_grade_level(raw_data[[grade_col]])
  }

  # Total enrollment
  all_col <- find_col(col_map$all_students)
  if (!is.null(all_col)) {
    result$row_total <- safe_numeric(raw_data[[all_col]])
  }

  # Gender counts
  gender_map <- list(
    male = col_map$male,
    female = col_map$female,
    gender_x = col_map$gender_x
  )

  for (name in names(gender_map)) {
    col <- find_col(gender_map[[name]])
    if (!is.null(col)) {
      result[[name]] <- safe_numeric(raw_data[[col]])
    }
  }

  # Race/Ethnicity counts
  race_map <- list(
    white = col_map$white,
    black = col_map$black,
    hispanic = col_map$hispanic,
    asian = col_map$asian,
    pacific_islander = col_map$pacific_islander,
    native_american = col_map$native_american,
    multiracial = col_map$multiracial
  )

  for (name in names(race_map)) {
    col <- find_col(race_map[[name]])
    if (!is.null(col)) {
      result[[name]] <- safe_numeric(raw_data[[col]])
    }
  }

  # Special populations
  special_map <- list(
    lep = col_map$ell,
    econ_disadv = col_map$low_income,
    special_ed = col_map$special_ed,
    homeless = col_map$homeless,
    foster_care = col_map$foster_care,
    migrant = col_map$migrant,
    military_parent = col_map$military_parent,
    section_504 = col_map$section_504,
    highly_capable = col_map$highly_capable,
    mobile = col_map$mobile
  )

  for (name in names(special_map)) {
    col <- find_col(special_map[[name]])
    if (!is.null(col)) {
      result[[name]] <- safe_numeric(raw_data[[col]])
    }
  }

  # Add grade-level totals by pivoting data
  # Washington data has one row per org/grade combination
  # We need to aggregate to get org-level totals with grade breakdowns
  result <- aggregate_grade_data(result, end_year)

  result
}


#' Standardize grade level strings
#'
#' Converts various grade level representations to standard format.
#'
#' @param grades Character vector of grade level strings
#' @return Character vector of standardized grade levels
#' @keywords internal
standardize_grade_level <- function(grades) {

  # Create mapping
  grade_map <- c(
    "Pre-Kindergarten" = "PK",
    "PreK" = "PK",
    "Pre-K" = "PK",
    "Kindergarten" = "K",
    "KG" = "K",
    "1st Grade" = "01",
    "Grade 1" = "01",
    "1" = "01",
    "2nd Grade" = "02",
    "Grade 2" = "02",
    "2" = "02",
    "3rd Grade" = "03",
    "Grade 3" = "03",
    "3" = "03",
    "4th Grade" = "04",
    "Grade 4" = "04",
    "4" = "04",
    "5th Grade" = "05",
    "Grade 5" = "05",
    "5" = "05",
    "6th Grade" = "06",
    "Grade 6" = "06",
    "6" = "06",
    "7th Grade" = "07",
    "Grade 7" = "07",
    "7" = "07",
    "8th Grade" = "08",
    "Grade 8" = "08",
    "8" = "08",
    "9th Grade" = "09",
    "Grade 9" = "09",
    "9" = "09",
    "10th Grade" = "10",
    "Grade 10" = "10",
    "11th Grade" = "11",
    "Grade 11" = "11",
    "12th Grade" = "12",
    "Grade 12" = "12",
    "All Grades" = "TOTAL",
    "AllGrades" = "TOTAL",
    "Half-day Kindergarten" = "K",
    "Half Day Kindergarten" = "K",
    "Full-day Kindergarten" = "K"
  )

  result <- grades

  for (pattern in names(grade_map)) {
    result[grepl(paste0("^", pattern, "$"), grades, ignore.case = TRUE)] <- grade_map[pattern]
  }

  result
}


#' Aggregate grade-level data to organization level
#'
#' Washington data has separate rows for each grade within an organization.
#' This function creates organization-level summary rows with grade breakdowns.
#'
#' @param df Processed data frame with grade-level rows
#' @param end_year School year end
#' @return Data frame with aggregated rows
#' @keywords internal
aggregate_grade_data <- function(df, end_year) {

  # Identify the organization key columns
  key_cols <- c("type", "district_id", "district_code", "district_name",
                "campus_id", "school_code", "campus_name", "school_type",
                "esd_id", "esd_name", "county")
  key_cols <- key_cols[key_cols %in% names(df)]

  # Check if we have grade-level data
  if (!"grade_level" %in% names(df)) {
    # No grade breakdown - return as is
    return(df)
  }

  # Numeric columns to aggregate
  num_cols <- c("row_total", "male", "female", "gender_x",
                "white", "black", "hispanic", "asian",
                "pacific_islander", "native_american", "multiracial",
                "lep", "econ_disadv", "special_ed", "homeless",
                "foster_care", "migrant", "military_parent",
                "section_504", "highly_capable", "mobile")
  num_cols <- num_cols[num_cols %in% names(df)]

  # If we have "All Grades" or "TOTAL" rows, those are the aggregates
  # Keep grade-specific data separate for grade-level analysis

  # Create organization-level summaries from "All Grades" rows
  all_grades_df <- df |>
    dplyr::filter(grade_level == "TOTAL" | is.na(grade_level))

  # Create grade-level data (excluding "All Grades")
  grade_df <- df |>
    dplyr::filter(grade_level != "TOTAL" & !is.na(grade_level))

  # If we have all-grades rows, use those as the main data
  if (nrow(all_grades_df) > 0) {
    # Pivot grade-level data to wide format for each organization
    if (nrow(grade_df) > 0) {
      grade_wide <- create_grade_columns(grade_df, key_cols)

      # Merge grade columns into all-grades data
      if (!is.null(grade_wide) && nrow(grade_wide) > 0) {
        all_grades_df <- dplyr::left_join(
          all_grades_df,
          grade_wide,
          by = key_cols
        )
      }
    }

    result <- all_grades_df
  } else {
    # No all-grades rows - aggregate from grade-level data
    result <- df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) |>
      dplyr::summarize(
        dplyr::across(dplyr::all_of(num_cols), ~sum(.x, na.rm = TRUE)),
        .groups = "drop"
      ) |>
      dplyr::mutate(end_year = end_year)

    # Add grade columns from grade-level data
    grade_wide <- create_grade_columns(grade_df, key_cols)
    if (!is.null(grade_wide) && nrow(grade_wide) > 0) {
      result <- dplyr::left_join(result, grade_wide, by = key_cols)
    }
  }

  # Clean up - remove grade_level column from final result
  result$grade_level <- NULL
  result$grade_level_raw <- NULL

  result
}


#' Create grade-level columns from grade rows
#'
#' Pivots grade-specific enrollment data to wide format with
#' grade_pk, grade_k, grade_01, etc. columns.
#'
#' @param grade_df Data frame with grade-level rows
#' @param key_cols Character vector of key columns for joining
#' @return Data frame with grade columns, or NULL if no data
#' @keywords internal
create_grade_columns <- function(grade_df, key_cols) {

  if (nrow(grade_df) == 0) {
    return(NULL)
  }

  # Only pivot the row_total for each grade
  if (!"row_total" %in% names(grade_df)) {
    return(NULL)
  }

  # Filter to valid grades and select needed columns
  grade_df <- grade_df |>
    dplyr::filter(grade_level %in% c("PK", "K", "01", "02", "03", "04", "05",
                                      "06", "07", "08", "09", "10", "11", "12")) |>
    dplyr::select(dplyr::all_of(c(key_cols, "grade_level", "row_total")))

  if (nrow(grade_df) == 0) {
    return(NULL)
  }

  # Pivot to wide format
  grade_wide <- grade_df |>
    tidyr::pivot_wider(
      names_from = grade_level,
      values_from = row_total,
      names_prefix = "grade_",
      values_fn = sum
    )

  # Standardize column names (grade_K -> grade_k, etc.)
  grade_cols <- grep("^grade_", names(grade_wide), value = TRUE)
  for (col in grade_cols) {
    new_name <- tolower(col)
    if (col != new_name) {
      names(grade_wide)[names(grade_wide) == col] <- new_name
    }
  }

  grade_wide
}


#' Create state-level aggregate from district data
#'
#' @param df Processed data frame (may already have state rows)
#' @param end_year School year end
#' @return Data frame with state aggregate added if not present
#' @keywords internal
create_state_aggregate <- function(df, end_year) {

  # Check if state row already exists
  if (any(df$type == "State", na.rm = TRUE)) {
    return(df)
  }

  # Get district-level rows
  district_df <- df |>
    dplyr::filter(type == "District")

  if (nrow(district_df) == 0) {
    return(df)
  }

  # Columns to sum
  sum_cols <- c(
    "row_total", "male", "female", "gender_x",
    "white", "black", "hispanic", "asian",
    "pacific_islander", "native_american", "multiracial",
    "lep", "econ_disadv", "special_ed", "homeless",
    "foster_care", "migrant", "military_parent",
    "section_504", "highly_capable", "mobile",
    "grade_pk", "grade_k",
    "grade_01", "grade_02", "grade_03", "grade_04",
    "grade_05", "grade_06", "grade_07", "grade_08",
    "grade_09", "grade_10", "grade_11", "grade_12"
  )

  # Filter to columns that exist
  sum_cols <- sum_cols[sum_cols %in% names(district_df)]

  # Create state row
  state_row <- data.frame(
    end_year = end_year,
    type = "State",
    district_id = NA_character_,
    district_code = NA_character_,
    district_name = NA_character_,
    campus_id = NA_character_,
    school_code = NA_character_,
    campus_name = NA_character_,
    county = NA_character_,
    stringsAsFactors = FALSE
  )

  # Sum each column
  for (col in sum_cols) {
    state_row[[col]] <- sum(district_df[[col]], na.rm = TRUE)
  }

  # Combine
  dplyr::bind_rows(state_row, df)
}
