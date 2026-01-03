# Tests for enrollment functions
# Note: Most tests are marked as skip_on_cran since they require network access

test_that("safe_numeric handles various inputs", {
  # Normal numbers
  expect_equal(safe_numeric("100"), 100)
  expect_equal(safe_numeric("1,234"), 1234)

  # Suppressed values (OSPI uses various markers)
  expect_true(is.na(safe_numeric("*")))
  expect_true(is.na(safe_numeric("<5")))
  expect_true(is.na(safe_numeric(">95")))
  expect_true(is.na(safe_numeric("N/A")))
  expect_true(is.na(safe_numeric("")))
  expect_true(is.na(safe_numeric("null")))

  # Whitespace handling
  expect_equal(safe_numeric("  100  "), 100)

  # Empty input
  expect_equal(length(safe_numeric(NULL)), 0)
  expect_equal(length(safe_numeric(character(0))), 0)
})

test_that("parse_school_year handles various formats", {
  # Standard format
  expect_equal(parse_school_year("2023-24"), 2024L)
  expect_equal(parse_school_year("2022-23"), 2023L)

  # Full year format
  expect_equal(parse_school_year("2023-2024"), 2024L)

  # Single year (assumed end year)
  expect_equal(parse_school_year("2024"), 2024L)

  # NULL and NA
  expect_true(is.na(parse_school_year(NULL)))
  expect_true(is.na(parse_school_year(NA)))
})

test_that("format_school_year converts end year to string", {
  expect_equal(format_school_year(2024), "2023-24")
  expect_equal(format_school_year(2025), "2024-25")
  expect_equal(format_school_year(2015), "2014-15")
})

test_that("get_available_years returns valid range", {
  years <- get_available_years()

  expect_true(is.numeric(years))
  expect_true(length(years) > 0)
  expect_true(min(years) >= 2010)  # Earliest available year

  expect_true(max(years) <= 2026)  # Reasonable upper bound
})

test_that("get_dataset_id returns IDs for known years", {
  # Report Card Enrollment dataset IDs (2015-2024)
  expect_equal(get_dataset_id(2024), "q4ba-s3jc")
  expect_equal(get_dataset_id(2023), "dij7-mbxg")
  expect_equal(get_dataset_id(2022), "ymi4-syjv")
  expect_equal(get_dataset_id(2021), "nvpc-yr7b")
  expect_equal(get_dataset_id(2020), "gtd3-scga")
  expect_equal(get_dataset_id(2019), "u4gd-6wxx")
  expect_equal(get_dataset_id(2018), "fs63-sd8y")
  expect_equal(get_dataset_id(2017), "y85c-tmgt")
  expect_equal(get_dataset_id(2016), "ajpq-2bg9")
  expect_equal(get_dataset_id(2015), "i9gq-g35m")

  # Student Enrollment dataset IDs (2010-2014)
  expect_equal(get_dataset_id(2014), "esyr-g8p5")
  expect_equal(get_dataset_id(2013), "9949-vk3e")
  expect_equal(get_dataset_id(2012), "5bjv-pebn")
  expect_equal(get_dataset_id(2011), "93ce-b95t")
  expect_equal(get_dataset_id(2010), "mpef-t92p")

  # Unknown year returns NULL
  expect_null(get_dataset_id(1999))
  expect_null(get_dataset_id(2050))
})

test_that("fetch_enr validates year parameter", {
  # Years outside the available range should error
  expect_error(fetch_enr(2009), "end_year must be between")  # Before 2010
  expect_error(fetch_enr(2030), "end_year must be between")  # After 2024
  expect_error(fetch_enr(1990), "end_year must be between")  # Way before available range
})

test_that("get_cache_dir returns valid path", {
  cache_dir <- get_cache_dir()
  expect_true(is.character(cache_dir))
  expect_true(grepl("waschooldata", cache_dir))
})

test_that("cache functions work correctly", {
  # Test cache path generation
  path <- get_cache_path(2024, "tidy")
  expect_true(grepl("enr_tidy_2024.rds", path))

  path_wide <- get_cache_path(2023, "wide")
  expect_true(grepl("enr_wide_2023.rds", path_wide))

  # Test cache_exists returns FALSE for non-existent cache
  expect_false(cache_exists(9999, "tidy"))
  expect_false(cache_exists(9999, "wide"))
})

test_that("standardize_grade_level converts grades correctly", {
  grades <- c("Kindergarten", "1st Grade", "Grade 5", "12th Grade", "All Grades")
  result <- standardize_grade_level(grades)

  expect_equal(result[1], "K")
  expect_equal(result[2], "01")
  expect_equal(result[3], "05")
  expect_equal(result[4], "12")
  expect_equal(result[5], "TOTAL")
})

test_that("clean_column_names standardizes names", {
  names <- c("SchoolYear", "All Students", "Hispanic/Latino")
  result <- clean_column_names(names)

  expect_equal(result[1], "schoolyear")
  expect_equal(result[2], "all_students")
  expect_equal(result[3], "hispanic_latino")
})

# Integration tests (require network access)
test_that("fetch_enr downloads and processes data", {
  skip_on_cran()
  skip_if_offline()

  # Use a recent year
  result <- fetch_enr(2024, tidy = FALSE, use_cache = FALSE)

  # Check structure
  expect_true(is.data.frame(result))
  expect_true("district_id" %in% names(result) || "district_code" %in% names(result))
  expect_true("row_total" %in% names(result))
  expect_true("type" %in% names(result))

  # Check we have all levels
  expect_true("State" %in% result$type)
  expect_true("District" %in% result$type)
  expect_true("Campus" %in% result$type || "School" %in% result$type)
})

test_that("tidy_enr produces correct long format", {
  skip_on_cran()
  skip_if_offline()

  # Get wide data
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)

  # Tidy it
  tidy_result <- tidy_enr(wide)

  # Check structure
  expect_true("grade_level" %in% names(tidy_result))
  expect_true("subgroup" %in% names(tidy_result))
  expect_true("n_students" %in% names(tidy_result))
  expect_true("pct" %in% names(tidy_result))

  # Check subgroups include expected values
  subgroups <- unique(tidy_result$subgroup)
  expect_true("total_enrollment" %in% subgroups)
  expect_true("hispanic" %in% subgroups || "hispanic_latino" %in% subgroups)
  expect_true("white" %in% subgroups)
})

test_that("id_enr_aggs adds correct flags", {
  skip_on_cran()
  skip_if_offline()

  # Get tidy data with aggregation flags
  result <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  # Check flags exist
  expect_true("is_state" %in% names(result))
  expect_true("is_district" %in% names(result))
  expect_true("is_campus" %in% names(result))

  # Check flags are boolean
  expect_true(is.logical(result$is_state))
  expect_true(is.logical(result$is_district))
  expect_true(is.logical(result$is_campus))

  # Check mutual exclusivity (each row is only one type)
  type_sums <- result$is_state + result$is_district + result$is_campus
  expect_true(all(type_sums == 1))
})

test_that("fetch_enr_multi combines multiple years", {
  skip_on_cran()
  skip_if_offline()

  # Fetch two years
  result <- fetch_enr_multi(c(2023, 2024), tidy = TRUE, use_cache = TRUE)

  # Check we have both years
  years <- unique(result$end_year)
  expect_true(2023 %in% years)
  expect_true(2024 %in% years)
})

test_that("enr_grade_aggs creates grade aggregates", {
  skip_on_cran()
  skip_if_offline()

  # Get tidy data
  tidy_data <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  # Create aggregates
  grade_aggs <- enr_grade_aggs(tidy_data)

  # Check grade levels include expected aggregates
  grade_levels <- unique(grade_aggs$grade_level)
  expect_true("K8" %in% grade_levels)
  expect_true("HS" %in% grade_levels)
  expect_true("K12" %in% grade_levels)
})

test_that("state totals are reasonable", {
  skip_on_cran()
  skip_if_offline()

  # Get 2024 data
  result <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  # Get state total enrollment
  state_total <- result %>%
    dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    dplyr::pull(n_students)

  # Washington has approximately 1.1 million K-12 students
  # Should be between 900K and 1.5M
  expect_true(state_total > 900000)
  expect_true(state_total < 1500000)
})

test_that("historical data (2010-2014) downloads and processes", {
  skip_on_cran()
  skip_if_offline()

  # Test a year from the Student Enrollment era
  result <- fetch_enr(2012, tidy = FALSE, use_cache = FALSE)

  # Check basic structure
  expect_true(is.data.frame(result))
  expect_true("row_total" %in% names(result))
  expect_true("type" %in% names(result))

  # 2010-2014 data has fewer columns than 2015+
  # These columns are NOT expected in 2010-2014 data:
  # district_id, district_code, campus_id, school_code, highly_capable
  # But we should still have the core enrollment data
  expect_true("white" %in% names(result) || "row_total" %in% names(result))

  # Check we have all levels
  expect_true("State" %in% result$type)
  expect_true("District" %in% result$type)
})

test_that("fetch_enr_multi spans both data eras", {
  skip_on_cran()
  skip_if_offline()

  # Fetch from both Student Enrollment (2014) and Report Card (2015) eras
  result <- fetch_enr_multi(c(2014, 2015), tidy = TRUE, use_cache = TRUE)

  # Check we have both years
  years <- unique(result$end_year)
  expect_true(2014 %in% years)
  expect_true(2015 %in% years)

  # Both years should have basic subgroups
  subgroups <- unique(result$subgroup)
  expect_true("total_enrollment" %in% subgroups)
  expect_true("white" %in% subgroups)
})
