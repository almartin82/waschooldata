# ==============================================================================
# LIVE Pipeline Tests for Washington Enrollment Data
# ==============================================================================
#
# These tests verify each step of the data pipeline using LIVE network calls.
# NO MOCKS - the goal is to detect breakages when state DOE websites change.
#
# Test Categories:
# 1. URL Availability Tests - Verify Socrata API endpoints return HTTP 200
# 2. API Download Tests - Verify data downloads completely
# 3. JSON Parsing Tests - Verify response parses correctly
# 4. Column Structure Tests - Verify expected columns exist
# 5. Year Filtering Tests - Verify data extraction by year
# 6. Aggregation Tests - Verify totals sum correctly
# 7. Data Quality Tests - Verify no Inf/NaN/impossible values
# 8. Output Fidelity Tests - Verify tidy output matches raw data
#
# ==============================================================================

# Helper: Skip if no network connectivity
skip_if_offline <- function() {
  tryCatch({
    response <- httr::HEAD("https://www.google.com", httr::timeout(5))
    if (httr::http_error(response)) skip("No network connectivity")
  }, error = function(e) skip("No network connectivity"))
}

# ==============================================================================
# 1. URL AVAILABILITY TESTS
# ==============================================================================

test_that("data.wa.gov base domain is accessible", {
  skip_on_cran()
  skip_if_offline()

  response <- httr::HEAD("https://data.wa.gov", httr::timeout(30))
  expect_equal(httr::status_code(response), 200)
})

test_that("Socrata API endpoint for 2024 dataset returns HTTP 200", {
  skip_on_cran()
  skip_if_offline()

  url <- "https://data.wa.gov/resource/q4ba-s3jc.json?$limit=1"
  response <- httr::GET(url, httr::timeout(30))
  expect_equal(httr::status_code(response), 200)
})

test_that("All known dataset IDs return HTTP 200", {
  skip_on_cran()
  skip_if_offline()

  # Dataset IDs by year (from get_dataset_id function)
  dataset_ids <- list(
    "2025" = "2rwv-gs2e",
    "2024" = "q4ba-s3jc",
    "2023" = "dij7-mbxg",
    "2022" = "ymi4-syjv",
    "2021" = "nvpc-yr7b",
    "2020" = "gtd3-scga",
    "2019" = "u4gd-6wxx",
    "2018" = "fs63-sd8y",
    "2017" = "y85c-tmgt",
    "2016" = "ajpq-2bg9",
    "2015" = "i9gq-g35m",
    "2014" = "esyr-g8p5",
    "2013" = "9949-vk3e",
    "2012" = "5bjv-pebn",
    "2011" = "93ce-b95t",
    "2010" = "mpef-t92p"
  )

  for (year in names(dataset_ids)) {
    id <- dataset_ids[[year]]
    url <- paste0("https://data.wa.gov/resource/", id, ".json?$limit=1")
    response <- httr::GET(url, httr::timeout(30))
    expect_equal(
      httr::status_code(response),
      200,
      info = paste("Year", year, "dataset", id, "should return HTTP 200")
    )
  }
})

# ==============================================================================
# 2. API DOWNLOAD TESTS
# ==============================================================================

test_that("Can download enrollment data for 2024", {
  skip_on_cran()
  skip_if_offline()

  url <- "https://data.wa.gov/resource/q4ba-s3jc.json?$limit=100"
  response <- httr::GET(url, httr::timeout(60))

  expect_equal(httr::status_code(response), 200)

  # Check we got actual data, not an error page
  content_type <- httr::headers(response)$`content-type`
  expect_true(grepl("application/json", content_type, ignore.case = TRUE))

  # Check response body is not empty
  body <- httr::content(response, "text", encoding = "UTF-8")
  expect_true(nchar(body) > 100, info = "Response should have substantial content")
})

test_that("Socrata API pagination works correctly", {
  skip_on_cran()
  skip_if_offline()

  # Get first batch
  url1 <- "https://data.wa.gov/resource/q4ba-s3jc.json?$limit=10&$offset=0"
  response1 <- httr::GET(url1, httr::timeout(30))
  batch1 <- jsonlite::fromJSON(httr::content(response1, "text", encoding = "UTF-8"))

  # Get second batch
  url2 <- "https://data.wa.gov/resource/q4ba-s3jc.json?$limit=10&$offset=10"
  response2 <- httr::GET(url2, httr::timeout(30))
  batch2 <- jsonlite::fromJSON(httr::content(response2, "text", encoding = "UTF-8"))

  expect_equal(nrow(batch1), 10)
  expect_equal(nrow(batch2), 10)

  # Batches should be different (different records)
  # Compare first record from each batch
  expect_false(identical(batch1[1, ], batch2[1, ]))
})

# ==============================================================================
# 3. JSON PARSING TESTS
# ==============================================================================

test_that("API response parses to valid JSON", {
  skip_on_cran()
  skip_if_offline()

  url <- "https://data.wa.gov/resource/q4ba-s3jc.json?$limit=5"
  response <- httr::GET(url, httr::timeout(30))
  content <- httr::content(response, "text", encoding = "UTF-8")

  # Should parse without error
  df <- jsonlite::fromJSON(content)

  expect_true(is.data.frame(df))
  expect_true(nrow(df) > 0, info = "Should return data rows")
  expect_true(ncol(df) > 0, info = "Should return columns")
})

test_that("Parsed data has expected basic structure", {
  skip_on_cran()
  skip_if_offline()

  url <- "https://data.wa.gov/resource/q4ba-s3jc.json?$limit=10"
  response <- httr::GET(url, httr::timeout(30))
  df <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))

  # Should have at least 30 columns (enrollment data is wide)
  expect_true(ncol(df) > 30, info = "Enrollment data should have many columns")

  # Should have enrollment count column
  expect_true("all_students" %in% names(df), info = "Should have all_students column")
})

# ==============================================================================
# 4. COLUMN STRUCTURE TESTS
# ==============================================================================

test_that("2024 dataset has expected core columns", {
  skip_on_cran()
  skip_if_offline()

  url <- "https://data.wa.gov/resource/q4ba-s3jc.json?$limit=1"
  response <- httr::GET(url, httr::timeout(30))
  df <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))

  expected_cols <- c(
    "schoolyear",
    "organizationlevel",
    "districtname",
    "gradelevel",
    "all_students"
  )

  for (col in expected_cols) {
    expect_true(col %in% names(df), info = paste("Should have column:", col))
  }
})

test_that("2024 dataset has demographic columns", {
  skip_on_cran()
  skip_if_offline()

  url <- "https://data.wa.gov/resource/q4ba-s3jc.json?$limit=1"
  response <- httr::GET(url, httr::timeout(30))
  df <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))

  # Gender columns
  gender_cols <- c("male", "female")
  for (col in gender_cols) {
    expect_true(col %in% names(df), info = paste("Should have gender column:", col))
  }

  # Race columns
  race_cols <- c("white", "asian", "hispanic_latino_of_any_race")
  for (col in race_cols) {
    expect_true(col %in% names(df), info = paste("Should have race column:", col))
  }
})

test_that("2024 dataset has special population columns", {
  skip_on_cran()
  skip_if_offline()

  url <- "https://data.wa.gov/resource/q4ba-s3jc.json?$limit=1"
  response <- httr::GET(url, httr::timeout(30))
  df <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))

  special_cols <- c(
    "english_language_learners",
    "low_income",
    "students_with_disabilities",
    "homeless",
    "foster_care"
  )

  for (col in special_cols) {
    expect_true(col %in% names(df), info = paste("Should have special population column:", col))
  }
})

# ==============================================================================
# 5. YEAR FILTERING TESTS
# ==============================================================================

test_that("get_raw_enr returns data for 2024", {
  skip_on_cran()
  skip_if_offline()

  raw <- waschooldata:::get_raw_enr(2024)

  expect_true(is.data.frame(raw))
  expect_true(nrow(raw) > 10000, info = "Should have >10k records for 2024")

  # Should have school year column
  expect_true("schoolyear" %in% names(raw) || "end_year" %in% names(raw))
})

test_that("get_raw_enr returns data for historical year (2015)", {

  skip_on_cran()
  skip_if_offline()

  raw <- waschooldata:::get_raw_enr(2015)

  expect_true(is.data.frame(raw))
  expect_true(nrow(raw) > 5000, info = "Should have >5k records for 2015")
})

test_that("get_raw_enr returns data for oldest supported year (2010)", {
  skip_on_cran()
  skip_if_offline()

  raw <- waschooldata:::get_raw_enr(2010)

  expect_true(is.data.frame(raw))
  expect_true(nrow(raw) > 5000, info = "Should have >5k records for 2010")
})

test_that("get_raw_enr errors for unsupported year", {
  skip_on_cran()
  skip_if_offline()

  expect_error(waschooldata:::get_raw_enr(2005), "not available")
  expect_error(waschooldata:::get_raw_enr(2030), "not available")
})

# ==============================================================================
# 6. AGGREGATION TESTS
# ==============================================================================

test_that("State total equals sum of district totals for 2024", {
  skip_on_cran()
  skip_if_offline()

  # Get state total directly from API
  url_state <- URLencode("https://data.wa.gov/resource/q4ba-s3jc.json?$where=organizationlevel='State' AND gradelevel='All Grades'")
  response_state <- httr::GET(url_state, httr::timeout(30))
  state_df <- jsonlite::fromJSON(httr::content(response_state, "text", encoding = "UTF-8"))
  state_total <- as.numeric(state_df$all_students[1])

  # Get sum of district totals from API
  url_districts <- URLencode("https://data.wa.gov/resource/q4ba-s3jc.json?$where=organizationlevel='District' AND gradelevel='All Grades'&$limit=50000")
  response_districts <- httr::GET(url_districts, httr::timeout(60))
  districts_df <- jsonlite::fromJSON(httr::content(response_districts, "text", encoding = "UTF-8"))
  district_sum <- sum(as.numeric(districts_df$all_students), na.rm = TRUE)

  # State should equal sum of districts (within 1% tolerance for rounding)
  expect_equal(state_total, district_sum, tolerance = state_total * 0.01,
               info = "State total should equal sum of district totals")
})

test_that("Gender totals approximately equal all_students for state", {
  skip_on_cran()
  skip_if_offline()

  # Get state row
  url <- URLencode("https://data.wa.gov/resource/q4ba-s3jc.json?$where=organizationlevel='State' AND gradelevel='All Grades'")
  response <- httr::GET(url, httr::timeout(30))
  df <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))

  total <- as.numeric(df$all_students[1])
  male <- as.numeric(df$male[1])
  female <- as.numeric(df$female[1])
  gender_x <- as.numeric(df$gender_x[1])

  gender_sum <- male + female + gender_x

  # Gender sum should equal total (within 1% for suppressed values)
  expect_equal(total, gender_sum, tolerance = total * 0.01,
               info = "Gender sum should equal all_students")
})

test_that("fetch_enr state total matches API state total", {
  skip_on_cran()
  skip_if_offline()

  # Get state total from API
  url <- URLencode("https://data.wa.gov/resource/q4ba-s3jc.json?$where=organizationlevel='State' AND gradelevel='All Grades'")
  response <- httr::GET(url, httr::timeout(30))
  api_df <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  api_total <- as.numeric(api_df$all_students[1])

  # Get state total from package
  processed <- fetch_enr(2024, tidy = FALSE, use_cache = FALSE)
  package_total <- processed$row_total[processed$type == "State"][1]

  expect_equal(api_total, package_total,
               info = "Package state total should match API state total")
})

# ==============================================================================
# 7. DATA QUALITY TESTS
# ==============================================================================

test_that("No Inf values in processed data", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)

  numeric_cols <- names(data)[sapply(data, is.numeric)]
  for (col in numeric_cols) {
    expect_false(
      any(is.infinite(data[[col]]), na.rm = TRUE),
      info = paste("Column", col, "should not have Inf values")
    )
  }
})

test_that("No NaN values in processed data", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)

  numeric_cols <- names(data)[sapply(data, is.numeric)]
  for (col in numeric_cols) {
    expect_false(
      any(is.nan(data[[col]]), na.rm = TRUE),
      info = paste("Column", col, "should not have NaN values")
    )
  }
})

test_that("All enrollment counts are non-negative", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)

  expect_true(
    all(data$row_total >= 0, na.rm = TRUE),
    info = "row_total should be non-negative"
  )

  if ("male" %in% names(data)) {
    expect_true(
      all(data$male >= 0, na.rm = TRUE),
      info = "male should be non-negative"
    )
  }

  if ("female" %in% names(data)) {
    expect_true(
      all(data$female >= 0, na.rm = TRUE),
      info = "female should be non-negative"
    )
  }
})

test_that("State total is in reasonable range", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  state_total <- data$row_total[data$type == "State"][1]

  # Washington has ~1.1 million students
  expect_true(state_total > 900000, info = "State total should be > 900k")
  expect_true(state_total < 1500000, info = "State total should be < 1.5M")
})

test_that("Percentages in tidy output are in valid range", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  if ("pct" %in% names(data)) {
    # Filter out NA values
    pct_values <- data$pct[!is.na(data$pct)]

    expect_true(
      all(pct_values >= 0, na.rm = TRUE),
      info = "Percentages should be >= 0"
    )

    expect_true(
      all(pct_values <= 1, na.rm = TRUE),
      info = "Percentages should be <= 1 (as decimal)"
    )
  }
})

test_that("No duplicate rows in processed output", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)

  # Key columns for uniqueness
  key_cols <- c("type", "district_id", "campus_id")
  key_cols <- key_cols[key_cols %in% names(data)]

  if (length(key_cols) > 0) {
    n_rows <- nrow(data)
    n_unique <- nrow(unique(data[, key_cols, drop = FALSE]))

    expect_equal(n_rows, n_unique, info = "Should have no duplicate rows")
  }
})

# ==============================================================================
# 8. OUTPUT FIDELITY TESTS
# ==============================================================================

test_that("tidy=TRUE state total matches tidy=FALSE state total", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  wide_state <- wide$row_total[wide$type == "State"][1]
  tidy_state <- tidy$n_students[tidy$is_state & tidy$subgroup == "total_enrollment" & tidy$grade_level == "TOTAL"][1]

  expect_equal(wide_state, tidy_state,
               info = "State total should match between wide and tidy formats")
})

test_that("tidy output has required columns", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  required_cols <- c("end_year", "type", "subgroup", "n_students", "is_state", "is_district", "is_campus")

  for (col in required_cols) {
    expect_true(col %in% names(tidy), info = paste("Tidy output should have column:", col))
  }
})

test_that("tidy output contains expected subgroups", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  subgroups <- unique(tidy$subgroup)

  expected_subgroups <- c("total_enrollment", "white", "male", "female")

  for (sg in expected_subgroups) {
    expect_true(sg %in% subgroups, info = paste("Should have subgroup:", sg))
  }
})

test_that("Male + Female approximately equals total for state", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  state_data <- tidy[tidy$is_state & tidy$grade_level == "TOTAL", ]

  total <- state_data$n_students[state_data$subgroup == "total_enrollment"]
  male <- state_data$n_students[state_data$subgroup == "male"]
  female <- state_data$n_students[state_data$subgroup == "female"]

  if (length(total) > 0 && length(male) > 0 && length(female) > 0) {
    gender_sum <- male[1] + female[1]
    # Allow for gender_x category
    expect_true(
      abs(total[1] - gender_sum) < total[1] * 0.05,
      info = "Male + Female should be close to total (within 5% for gender_x)"
    )
  }
})

test_that("Race categories sum approximately to total for state", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  state_data <- tidy[tidy$is_state & tidy$grade_level == "TOTAL", ]

  total <- state_data$n_students[state_data$subgroup == "total_enrollment"][1]

  race_subgroups <- c("white", "black", "hispanic", "asian", "native_american", "pacific_islander", "multiracial")
  race_sums <- sapply(race_subgroups, function(sg) {
    val <- state_data$n_students[state_data$subgroup == sg]
    if (length(val) > 0) val[1] else 0
  })

  race_total <- sum(race_sums, na.rm = TRUE)

  # Race total should be close to all_students (within 5% tolerance)
  expect_true(
    abs(total - race_total) < total * 0.05,
    info = paste("Race sum (", race_total, ") should be close to total (", total, ")")
  )
})

# ==============================================================================
# CROSS-YEAR CONSISTENCY TESTS
# ==============================================================================

test_that("State totals are consistent across recent years", {
  skip_on_cran()
  skip_if_offline()

  years <- 2022:2024
  totals <- sapply(years, function(yr) {
    data <- fetch_enr(yr, tidy = FALSE, use_cache = TRUE)
    data$row_total[data$type == "State"][1]
  })

  # Year-over-year change should be < 10%
  for (i in 2:length(totals)) {
    yoy_change <- abs(totals[i] / totals[i-1] - 1)
    expect_true(
      yoy_change < 0.10,
      info = paste("YoY change from", years[i-1], "to", years[i], "should be < 10%")
    )
  }
})

test_that("Number of districts is consistent across recent years", {
  skip_on_cran()
  skip_if_offline()

  years <- 2022:2024
  district_counts <- sapply(years, function(yr) {
    data <- fetch_enr(yr, tidy = FALSE, use_cache = TRUE)
    sum(data$type == "District", na.rm = TRUE)
  })

  # District count should be similar across years (within 10%)
  for (i in 2:length(district_counts)) {
    change <- abs(district_counts[i] / district_counts[i-1] - 1)
    expect_true(
      change < 0.10,
      info = paste("District count change from", years[i-1], "to", years[i], "should be < 10%")
    )
  }
})
