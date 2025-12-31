# Tests for caching functions

test_that("cache directory is created correctly", {
  cache_dir <- get_cache_dir()

  expect_true(is.character(cache_dir))
  expect_true(dir.exists(cache_dir))
  expect_true(grepl("waschooldata", cache_dir))
})

test_that("cache path generation is correct", {
  path_tidy <- get_cache_path(2024, "tidy")
  path_wide <- get_cache_path(2024, "wide")

  expect_true(grepl("enr_tidy_2024.rds$", path_tidy))
  expect_true(grepl("enr_wide_2024.rds$", path_wide))
  expect_true(grepl("waschooldata", path_tidy))
})

test_that("cache_exists returns FALSE for non-existent files", {
  # Use a year that definitely doesn't have cached data
  expect_false(cache_exists(1900, "tidy"))
  expect_false(cache_exists(1900, "wide"))
})

test_that("cache read/write roundtrip works", {
  # Create test data
  test_data <- data.frame(
    year = 9998,
    value = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  # Write to cache
  write_cache(test_data, 9998, "test")

  # Check exists
  expect_true(cache_exists(9998, "test"))

  # Read back
  read_data <- read_cache(9998, "test")
  expect_equal(read_data, test_data)

  # Clean up
  clear_cache(9998, "test")
  expect_false(cache_exists(9998, "test"))
})

test_that("clear_cache removes files correctly", {
  # Create test files
  test_data <- data.frame(x = 1)
  write_cache(test_data, 9997, "tidy")
  write_cache(test_data, 9997, "wide")
  write_cache(test_data, 9996, "tidy")

  # Clear specific file
  clear_cache(9997, "tidy")
  expect_false(cache_exists(9997, "tidy"))
  expect_true(cache_exists(9997, "wide"))

  # Clear all for year
  write_cache(test_data, 9997, "tidy")
  clear_cache(9997)
  expect_false(cache_exists(9997, "tidy"))
  expect_false(cache_exists(9997, "wide"))

  # Clean up remaining
  clear_cache(9996)
})

test_that("cache_status runs without error", {
  # Should not error even with empty cache
  expect_silent(result <- cache_status())
  expect_true(is.data.frame(result) || is.null(result))
})

test_that("cache respects max_age parameter", {
  # Create test file
  test_data <- data.frame(x = 1)
  write_cache(test_data, 9995, "test")

  # Should exist with default max_age

  expect_true(cache_exists(9995, "test", max_age = 30))

  # Should exist with max_age of 0 (just created)
  expect_true(cache_exists(9995, "test", max_age = 0.001))

  # Clean up
  clear_cache(9995, "test")
})
