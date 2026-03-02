# ==============================================================================
# LIVE Pipeline Tests for Washington Directory Data
# ==============================================================================
#
# These tests verify each step of the directory data pipeline using LIVE
# network calls. NO MOCKS.
#
# Test Categories:
# 1. URL Availability - HTTP 200 checks for data sources
# 2. Entity Data - Verify school/district entity download
# 3. EDS Directory - Verify EDS page scraping
# 4. Column Structure - Verify expected columns exist
# 5. Data Quality - No Inf/NaN/impossible values
# 6. fetch_directory() - Full pipeline
# 7. Caching - Verify cache functions work
#
# ==============================================================================

# Helper: Skip if no network connectivity
skip_if_offline_dir <- function() {
  tryCatch({
    response <- httr::HEAD("https://www.google.com", httr::timeout(5))
    if (httr::http_error(response)) skip("No network connectivity")
  }, error = function(e) skip("No network connectivity"))
}

# ==============================================================================
# 1. URL AVAILABILITY TESTS
# ==============================================================================

test_that("data.wa.gov enrollment API is accessible for directory", {
  skip_on_cran()
  skip_if_offline_dir()

  # Use the most recent enrollment dataset
  url <- "https://data.wa.gov/resource/2rwv-gs2e.json?$limit=1"
  response <- httr::GET(url, httr::timeout(30))
  expect_equal(httr::status_code(response), 200)
})

test_that("EDS directory page is accessible", {
  skip_on_cran()
  skip_if_offline_dir()

  response <- httr::GET(
    "https://eds.ospi.k12.wa.us/directoryeds.aspx",
    httr::timeout(30)
  )
  expect_equal(httr::status_code(response), 200)
})

# ==============================================================================
# 2. ENTITY DATA TESTS
# ==============================================================================

test_that("get_directory_entities returns data", {
  skip_on_cran()
  skip_if_offline_dir()

  entities <- waschooldata:::get_directory_entities(2025)

  expect_true(is.data.frame(entities))
  expect_true(nrow(entities) > 100, info = "Should have > 100 entities")
})

test_that("entity data includes districts and schools", {
  skip_on_cran()
  skip_if_offline_dir()

  entities <- waschooldata:::get_directory_entities(2025)

  # Normalize column names
  names(entities) <- tolower(names(entities))

  org_col <- if ("organizationlevel" %in% names(entities)) {
    "organizationlevel"
  } else {
    "organization_level"
  }

  org_levels <- unique(entities[[org_col]])

  expect_true("District" %in% org_levels, info = "Should have District records")
  expect_true("School" %in% org_levels, info = "Should have School records")
})

test_that("entity data has expected columns", {
  skip_on_cran()
  skip_if_offline_dir()

  entities <- waschooldata:::get_directory_entities(2025)
  col_names <- tolower(names(entities))

  # Check for key columns
  expect_true("districtname" %in% col_names || "district_name" %in% col_names,
              info = "Should have district name column")
  expect_true("schoolname" %in% col_names || "school_name" %in% col_names,
              info = "Should have school name column")
  expect_true("county" %in% col_names,
              info = "Should have county column")
})

# ==============================================================================
# 3. EDS DIRECTORY TESTS
# ==============================================================================

test_that("get_eds_directory returns data", {
  skip_on_cran()
  skip_if_offline_dir()

  eds <- waschooldata:::get_eds_directory()

  expect_true(is.data.frame(eds))
  expect_true(nrow(eds) > 200, info = "Should have > 200 districts from EDS")
})

test_that("EDS data has contact columns", {
  skip_on_cran()
  skip_if_offline_dir()

  eds <- waschooldata:::get_eds_directory()

  expected_cols <- c("eds_district_name", "eds_administrator", "eds_phone", "eds_email")
  for (col in expected_cols) {
    expect_true(col %in% names(eds), info = paste("EDS should have column:", col))
  }
})

test_that("EDS data has valid district names", {
  skip_on_cran()
  skip_if_offline_dir()

  eds <- waschooldata:::get_eds_directory()

  # Should have real district names
  expect_true(
    any(grepl("Seattle", eds$eds_district_name, ignore.case = TRUE)),
    info = "EDS should contain Seattle"
  )
  expect_true(
    any(grepl("Spokane", eds$eds_district_name, ignore.case = TRUE)),
    info = "EDS should contain Spokane"
  )
})

test_that("EDS data has non-empty phone numbers", {
  skip_on_cran()
  skip_if_offline_dir()

  eds <- waschooldata:::get_eds_directory()

  non_na_phones <- eds$eds_phone[!is.na(eds$eds_phone)]
  expect_true(
    length(non_na_phones) > 100,
    info = "Most districts should have phone numbers"
  )
})

# ==============================================================================
# 4. FULL PIPELINE TESTS
# ==============================================================================

test_that("fetch_directory returns data with tidy=TRUE", {
  skip_on_cran()
  skip_if_offline_dir()

  dir_data <- fetch_directory(end_year = 2025, tidy = TRUE, use_cache = FALSE)

  expect_true(is.data.frame(dir_data))
  expect_true(nrow(dir_data) > 100)
})

test_that("fetch_directory has expected columns in tidy mode", {
  skip_on_cran()
  skip_if_offline_dir()

  dir_data <- fetch_directory(end_year = 2025, tidy = TRUE, use_cache = TRUE)

  expected_cols <- c(
    "end_year", "entity_type",
    "state_district_id", "district_name",
    "school_name", "school_type",
    "county_name", "esd_name", "state"
  )

  for (col in expected_cols) {
    expect_true(
      col %in% names(dir_data),
      info = paste("Tidy directory should have column:", col)
    )
  }
})

test_that("fetch_directory has both districts and schools", {
  skip_on_cran()
  skip_if_offline_dir()

  dir_data <- fetch_directory(end_year = 2025, tidy = TRUE, use_cache = TRUE)

  entity_types <- unique(dir_data$entity_type)
  expect_true("District" %in% entity_types, info = "Should have District rows")
  expect_true("School" %in% entity_types, info = "Should have School rows")
})

test_that("fetch_directory returns data with tidy=FALSE", {
  skip_on_cran()
  skip_if_offline_dir()

  dir_raw <- fetch_directory(end_year = 2025, tidy = FALSE, use_cache = FALSE)

  expect_true(is.data.frame(dir_raw))
  expect_true(nrow(dir_raw) > 100)
})

test_that("fetch_directory defaults to most recent year", {
  skip_on_cran()
  skip_if_offline_dir()

  dir_data <- fetch_directory(use_cache = TRUE)

  expect_true(is.data.frame(dir_data))
  expect_true(nrow(dir_data) > 100)
})

# ==============================================================================
# 5. DATA QUALITY TESTS
# ==============================================================================

test_that("directory data has reasonable district count", {
  skip_on_cran()
  skip_if_offline_dir()

  dir_data <- fetch_directory(end_year = 2025, tidy = TRUE, use_cache = TRUE)

  n_districts <- sum(dir_data$entity_type == "District", na.rm = TRUE)

  # Washington has ~295 school districts
  expect_true(n_districts > 250, info = "Should have > 250 districts")
  expect_true(n_districts < 400, info = "Should have < 400 districts")
})

test_that("directory data has reasonable school count", {
  skip_on_cran()
  skip_if_offline_dir()

  dir_data <- fetch_directory(end_year = 2025, tidy = TRUE, use_cache = TRUE)

  n_schools <- sum(dir_data$entity_type == "School", na.rm = TRUE)

  # Washington has ~2,500 schools
  expect_true(n_schools > 1500, info = "Should have > 1500 schools")
  expect_true(n_schools < 5000, info = "Should have < 5000 schools")
})

test_that("no duplicate entities in directory", {
  skip_on_cran()
  skip_if_offline_dir()

  dir_data <- fetch_directory(end_year = 2025, tidy = TRUE, use_cache = TRUE)

  # Schools should be unique by state_school_id

  schools <- dir_data[dir_data$entity_type == "School" &
                        !is.na(dir_data$state_school_id), ]
  n_unique_schools <- length(unique(schools$state_school_id))
  expect_equal(nrow(schools), n_unique_schools,
               info = "Schools should be unique by state_school_id")

  # Districts should be unique by state_district_id
  districts <- dir_data[dir_data$entity_type == "District" &
                          !is.na(dir_data$state_district_id), ]
  n_unique_districts <- length(unique(districts$state_district_id))
  expect_equal(nrow(districts), n_unique_districts,
               info = "Districts should be unique by state_district_id")
})

test_that("all schools belong to a known district", {
  skip_on_cran()
  skip_if_offline_dir()

  dir_data <- fetch_directory(end_year = 2025, tidy = TRUE, use_cache = TRUE)

  schools <- dir_data[dir_data$entity_type == "School", ]
  districts <- dir_data[dir_data$entity_type == "District", ]

  # Every school's district_name should appear in the district list
  school_districts <- unique(schools$district_name[!is.na(schools$district_name)])
  known_districts <- unique(districts$district_name[!is.na(districts$district_name)])

  orphan_schools <- setdiff(school_districts, known_districts)

  # Allow small number of mismatches (ESD programs, etc.)
  expect_true(
    length(orphan_schools) < 10,
    info = paste(
      "Most schools should belong to a known district.",
      "Orphans:", paste(head(orphan_schools), collapse = ", ")
    )
  )
})

test_that("county_name values are real WA counties", {
  skip_on_cran()
  skip_if_offline_dir()

  dir_data <- fetch_directory(end_year = 2025, tidy = TRUE, use_cache = TRUE)

  # Washington has 39 counties
  counties <- unique(dir_data$county_name[!is.na(dir_data$county_name)])

  # Some well-known WA counties should be present
  expect_true("King" %in% counties, info = "Should have King County")
  expect_true("Pierce" %in% counties, info = "Should have Pierce County")
  expect_true("Spokane" %in% counties, info = "Should have Spokane County")
})

test_that("EDS contact info is merged for districts", {
  skip_on_cran()
  skip_if_offline_dir()

  dir_data <- fetch_directory(end_year = 2025, tidy = TRUE, use_cache = TRUE)

  districts <- dir_data[dir_data$entity_type == "District", ]

  # At least some districts should have phone numbers
  n_with_phone <- sum(!is.na(districts$phone))
  expect_true(
    n_with_phone > 100,
    info = paste("At least 100 districts should have phones. Got:", n_with_phone)
  )

  # Schools should NOT have contact info
  schools <- dir_data[dir_data$entity_type == "School", ]
  expect_true(
    all(is.na(schools$phone)),
    info = "Schools should not have phone numbers (EDS is district-only)"
  )
})

# ==============================================================================
# 6. CACHING TESTS
# ==============================================================================

test_that("directory cache write and read works", {
  skip_on_cran()
  skip_if_offline_dir()

  # Create small test dataframe
  test_df <- data.frame(
    entity_type = "District",
    district_name = "Test District",
    stringsAsFactors = FALSE
  )

  # Write to cache
  waschooldata:::write_cache_directory(test_df, 9999, "directory_test")

  # Read from cache
  expect_true(waschooldata:::cache_exists_directory(9999, "directory_test"))
  cached <- waschooldata:::read_cache_directory(9999, "directory_test")
  expect_equal(nrow(cached), 1)
  expect_equal(cached$district_name, "Test District")

  # Clean up
  cache_path <- waschooldata:::build_cache_path_directory(9999, "directory_test")
  if (file.exists(cache_path)) file.remove(cache_path)
})

test_that("clear_directory_cache removes files", {
  skip_on_cran()
  skip_if_offline_dir()

  # Write a test cache file
  test_df <- data.frame(x = 1)
  waschooldata:::write_cache_directory(test_df, 9998, "directory_cleanup_test")

  # Verify it exists
  cache_path <- waschooldata:::build_cache_path_directory(9998, "directory_cleanup_test")
  expect_true(file.exists(cache_path))

  # Clear and verify
  clear_directory_cache()

  # The test file should be removed (it starts with "directory_")
  expect_false(file.exists(cache_path))
})

test_that("fetch_directory uses cache on second call", {
  skip_on_cran()
  skip_if_offline_dir()

  # First call - downloads
  t1 <- system.time({
    dir1 <- fetch_directory(end_year = 2025, tidy = TRUE, use_cache = TRUE)
  })

  # Second call - should use cache (faster)
  t2 <- system.time({
    dir2 <- fetch_directory(end_year = 2025, tidy = TRUE, use_cache = TRUE)
  })

  expect_equal(nrow(dir1), nrow(dir2))

  # Cache should be substantially faster
  # (Only check if first call wasn't also cached)
  if (t1["elapsed"] > 2) {
    expect_true(
      t2["elapsed"] < t1["elapsed"],
      info = "Cached call should be faster than download"
    )
  }
})
