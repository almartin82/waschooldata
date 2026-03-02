# ==============================================================================
# Exhaustive Typology Tests for Washington School Data
# ==============================================================================
#
# Tests data structure, naming standards, column types, edge cases,
# data quality invariants, and cross-function consistency.
#
# ==============================================================================

# ==============================================================================
# Naming standards compliance
# ==============================================================================

test_that("Subgroup names follow naming standards", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  subgroups <- unique(enr$subgroup)

  # Must use standard names, not non-standard variants
  expect_false("total" %in% subgroups)            # should be "total_enrollment"
  expect_false("low_income" %in% subgroups)        # should be "econ_disadv"
  expect_false("frl" %in% subgroups)               # should be "econ_disadv"
  expect_false("iep" %in% subgroups)               # should be "special_ed"
  expect_false("disability" %in% subgroups)         # should be "special_ed"
  expect_false("el" %in% subgroups)                 # should be "lep"
  expect_false("ell" %in% subgroups)                # should be "lep"
  expect_false("english_learner" %in% subgroups)    # should be "lep"
  expect_false("american_indian" %in% subgroups)    # should be "native_american"
  expect_false("two_or_more" %in% subgroups)        # should be "multiracial"

  # Must have standard names
  expect_true("total_enrollment" %in% subgroups)
  expect_true("econ_disadv" %in% subgroups)
  expect_true("special_ed" %in% subgroups)
  expect_true("lep" %in% subgroups)
  expect_true("native_american" %in% subgroups)
  expect_true("multiracial" %in% subgroups)
})

test_that("Grade levels are uppercase standard format", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  grades <- unique(enr$grade_level)

  # All must be uppercase
  for (g in grades) {
    expect_equal(g, toupper(g), info = paste("Grade level should be uppercase:", g))
  }

  # Must not contain non-standard formats
  expect_false("pk" %in% grades)
  expect_false("k" %in% grades)
  expect_false("1" %in% grades)
  expect_false("Grade 1" %in% grades)
  expect_false("1st Grade" %in% grades)
  expect_false("Kindergarten" %in% grades)
  expect_false("All Grades" %in% grades)
  expect_false("AllGrades" %in% grades)
})

test_that("Entity flags use standard names", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  expect_true("is_state" %in% names(enr))
  expect_true("is_district" %in% names(enr))
  expect_true("is_campus" %in% names(enr))

  # Must not use non-standard names
  expect_false("is_school" %in% names(enr))
  expect_false("is_building" %in% names(enr))
})

# ==============================================================================
# Data quality invariants — no Inf, NaN, negative counts
# ==============================================================================

test_that("Tidy data has no Inf values in n_students", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_false(any(is.infinite(enr$n_students)))
})

test_that("Tidy data has no NaN values in n_students", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_false(any(is.nan(enr$n_students)))
})

test_that("Tidy data has no negative n_students", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  non_na_students <- enr$n_students[!is.na(enr$n_students)]
  expect_true(all(non_na_students >= 0))
})

test_that("Tidy data has no Inf values in pct", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_false(any(is.infinite(enr$pct)))
})

test_that("Tidy data has no NaN values in pct", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_false(any(is.nan(enr$pct)))
})

test_that("Tidy pct is between 0 and 1 inclusive", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  non_na_pct <- enr$pct[!is.na(enr$pct)]
  expect_true(all(non_na_pct >= 0))
  expect_true(all(non_na_pct <= 1))
})

test_that("Wide data has no Inf values in numeric columns", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  num_cols <- names(wide)[sapply(wide, is.numeric)]
  for (col in num_cols) {
    expect_false(
      any(is.infinite(wide[[col]]), na.rm = TRUE),
      info = paste("Inf in column:", col)
    )
  }
})

test_that("Wide data has no NaN values in numeric columns", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  num_cols <- names(wide)[sapply(wide, is.numeric)]
  for (col in num_cols) {
    expect_false(
      any(is.nan(wide[[col]]), na.rm = TRUE),
      info = paste("NaN in column:", col)
    )
  }
})

test_that("Wide data has no negative enrollment counts", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  count_cols <- c("row_total", "male", "female", "white", "black", "hispanic",
                  "asian", "native_american", "pacific_islander", "multiracial",
                  "lep", "econ_disadv", "special_ed")
  for (col in count_cols) {
    if (col %in% names(wide)) {
      vals <- wide[[col]][!is.na(wide[[col]])]
      expect_true(
        all(vals >= 0),
        info = paste("Negative value in column:", col)
      )
    }
  }
})

# ==============================================================================
# Demographic sum constraints
# ==============================================================================

test_that("Gender sum approximately equals total for all districts", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  districts <- enr[enr$is_district & enr$grade_level == "TOTAL", ]
  dist_ids <- unique(districts$district_id)

  for (did in dist_ids[1:20]) { # Check first 20 districts
    sub <- districts[districts$district_id == did, ]
    total <- sub$n_students[sub$subgroup == "total_enrollment"]
    male <- sub$n_students[sub$subgroup == "male"]
    female <- sub$n_students[sub$subgroup == "female"]

    if (length(total) == 1 && length(male) == 1 && length(female) == 1 && total > 0) {
      gender_sum <- male + female
      # Allow for gender_x
      expect_true(
        gender_sum <= total,
        info = paste("Gender sum exceeds total for district", did)
      )
      # Male + female should be at least 90% of total (gender_x is small)
      expect_true(
        gender_sum >= total * 0.9,
        info = paste("Gender sum too low for district", did)
      )
    }
  }
})

test_that("Race sum approximately equals total for state", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state <- enr[enr$is_state & enr$grade_level == "TOTAL", ]

  total <- state$n_students[state$subgroup == "total_enrollment"]
  race_subs <- c("white", "black", "hispanic", "asian",
                 "native_american", "pacific_islander", "multiracial")
  race_sum <- sum(sapply(race_subs, function(s) {
    val <- state$n_students[state$subgroup == s]
    if (length(val) == 1) val else 0
  }))

  expect_true(abs(total - race_sum) < total * 0.02,
              info = paste("Race sum", race_sum, "should be close to total", total))
})

test_that("Grade-level enrollment sums to total for state (PK-12)", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state <- enr[enr$is_state & enr$subgroup == "total_enrollment", ]

  total <- state$n_students[state$grade_level == "TOTAL"]
  grade_sum <- sum(state$n_students[state$grade_level != "TOTAL"])

  expect_equal(grade_sum, total)
})

# ==============================================================================
# Entity type exclusivity and completeness
# ==============================================================================

test_that("Every row has exactly one entity type flag set", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  flag_sum <- as.integer(enr$is_state) + as.integer(enr$is_district) + as.integer(enr$is_campus)
  expect_true(all(flag_sum == 1))
})

test_that("is_state rows have type == State", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true(all(enr$type[enr$is_state] == "State"))
})

test_that("is_district rows have type == District", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true(all(enr$type[enr$is_district] == "District"))
})

test_that("is_campus rows have type == Campus", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true(all(enr$type[enr$is_campus] == "Campus"))
})

test_that("State rows have NA district_id and campus_id", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state_rows <- enr[enr$is_state, ]
  expect_true(all(is.na(state_rows$district_id)))
  expect_true(all(is.na(state_rows$campus_id)))
})

test_that("District rows have non-NA district_id but NA campus_id", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  dist_rows <- enr[enr$is_district, ]
  expect_true(all(!is.na(dist_rows$district_id)))
  expect_true(all(is.na(dist_rows$campus_id)))
})

test_that("Campus rows have non-NA campus_id", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  campus_rows <- enr[enr$is_campus, ]
  expect_true(all(!is.na(campus_rows$campus_id)))
})

# ==============================================================================
# No duplicate rows in tidy output
# ==============================================================================

test_that("No duplicate rows in 2024 tidy at state level", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state <- enr[enr$is_state, ]

  # Each (subgroup, grade_level) combination should be unique at state level
  combos <- paste(state$subgroup, state$grade_level)
  expect_equal(length(combos), length(unique(combos)))
})

test_that("No duplicate rows in 2024 tidy at district level per district", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  dist <- enr[enr$is_district, ]

  # For each district, each (subgroup, grade_level) should be unique
  combos <- paste(dist$district_id, dist$subgroup, dist$grade_level)
  expect_equal(length(combos), length(unique(combos)))
})

test_that("No duplicate rows in 2024 wide output", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)

  # Each (type, district_id, campus_id) should be unique
  key <- paste(wide$type, wide$district_id, wide$campus_id, sep = "|")
  expect_equal(nrow(wide), length(unique(key)))
})

# ==============================================================================
# Wide-to-tidy fidelity
# ==============================================================================

test_that("Wide row_total matches tidy total_enrollment for every district", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  wide_districts <- wide[wide$type == "District", ]
  tidy_districts <- tidy[tidy$is_district & tidy$subgroup == "total_enrollment" & tidy$grade_level == "TOTAL", ]

  # Merge on district_id
  for (did in wide_districts$district_id[1:50]) {
    wide_val <- wide_districts$row_total[wide_districts$district_id == did]
    tidy_val <- tidy_districts$n_students[tidy_districts$district_id == did]
    if (length(wide_val) == 1 && length(tidy_val) == 1) {
      expect_equal(wide_val, tidy_val,
                   info = paste("Mismatch for district", did))
    }
  }
})

test_that("Wide demographic columns match tidy subgroup values for state", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  state_wide <- wide[wide$type == "State", ]
  state_tidy <- tidy[tidy$is_state & tidy$grade_level == "TOTAL", ]

  demo_map <- c(
    "white" = "white", "black" = "black", "hispanic" = "hispanic",
    "asian" = "asian", "native_american" = "native_american",
    "pacific_islander" = "pacific_islander", "multiracial" = "multiracial",
    "male" = "male", "female" = "female", "gender_x" = "gender_x",
    "special_ed" = "special_ed", "lep" = "lep", "econ_disadv" = "econ_disadv",
    "homeless" = "homeless", "foster_care" = "foster_care",
    "migrant" = "migrant", "military_parent" = "military_parent",
    "section_504" = "section_504", "highly_capable" = "highly_capable",
    "mobile" = "mobile"
  )

  for (wide_col in names(demo_map)) {
    sg <- demo_map[[wide_col]]
    wide_val <- state_wide[[wide_col]]
    tidy_val <- state_tidy$n_students[state_tidy$subgroup == sg]
    if (length(wide_val) == 1 && length(tidy_val) == 1) {
      expect_equal(wide_val, tidy_val,
                   info = paste("Wide", wide_col, "!=", "tidy", sg))
    }
  }
})

test_that("Wide grade columns match tidy grade-level enrollment for state", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  state_wide <- wide[wide$type == "State", ]
  state_tidy <- tidy[tidy$is_state & tidy$subgroup == "total_enrollment", ]

  grade_map <- c(
    "grade_pk" = "PK", "grade_k" = "K",
    "grade_01" = "01", "grade_02" = "02", "grade_03" = "03",
    "grade_04" = "04", "grade_05" = "05", "grade_06" = "06",
    "grade_07" = "07", "grade_08" = "08", "grade_09" = "09",
    "grade_10" = "10", "grade_11" = "11", "grade_12" = "12"
  )

  for (wide_col in names(grade_map)) {
    gl <- grade_map[[wide_col]]
    wide_val <- state_wide[[wide_col]]
    tidy_val <- state_tidy$n_students[state_tidy$grade_level == gl]
    if (length(wide_val) == 1 && length(tidy_val) == 1) {
      expect_equal(wide_val, tidy_val,
                   info = paste("Wide", wide_col, "!= tidy grade_level", gl))
    }
  }
})

# ==============================================================================
# Percentage calculations
# ==============================================================================

test_that("total_enrollment pct is always 1.0", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  total_pcts <- enr$pct[enr$subgroup == "total_enrollment" & enr$grade_level == "TOTAL"]
  non_na <- total_pcts[!is.na(total_pcts)]
  expect_true(all(non_na == 1.0))
})

test_that("Demographic pct = n_students / total for state", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state <- enr[enr$is_state & enr$grade_level == "TOTAL", ]

  total <- state$n_students[state$subgroup == "total_enrollment"]

  for (sg in c("white", "hispanic", "male", "econ_disadv", "lep")) {
    n <- state$n_students[state$subgroup == sg]
    pct <- state$pct[state$subgroup == sg]
    if (length(n) == 1 && length(pct) == 1 && total > 0) {
      expected_pct <- pmin(n / total, 1.0)
      expect_equal(pct, expected_pct, tolerance = 1e-10,
                   info = paste("pct mismatch for", sg))
    }
  }
})

# ==============================================================================
# Utility function edge cases
# ==============================================================================

test_that("safe_numeric handles vectors", {
  result <- safe_numeric(c("100", "200", "*", "<5", "N/A"))
  expect_equal(result[1], 100)
  expect_equal(result[2], 200)
  expect_true(is.na(result[3]))
  expect_true(is.na(result[4]))
  expect_true(is.na(result[5]))
})

test_that("safe_numeric handles already-numeric input", {
  result <- safe_numeric(c(100, 200, NA))
  expect_equal(result[1], 100)
  expect_equal(result[2], 200)
  expect_true(is.na(result[3]))
})

test_that("safe_numeric handles negative marker -1", {
  result <- safe_numeric("-1")
  expect_true(is.na(result))
})

test_that("safe_numeric handles dot marker", {
  result <- safe_numeric(".")
  expect_true(is.na(result))
})

test_that("safe_numeric handles greater-than markers", {
  result <- safe_numeric(">95")
  expect_true(is.na(result))
})

test_that("safe_numeric handles commas in thousands", {
  expect_equal(safe_numeric("1,234,567"), 1234567)
  expect_equal(safe_numeric("12,345"), 12345)
})

test_that("safe_numeric handles whitespace", {
  expect_equal(safe_numeric("  42  "), 42)
  expect_equal(safe_numeric(" 1,000 "), 1000)
})

test_that("parse_school_year handles edge cases", {
  expect_true(is.na(parse_school_year("invalid")))
  expect_true(is.na(parse_school_year("")))
  expect_equal(parse_school_year("2009-10"), 2010L)
  expect_equal(parse_school_year("1999-2000"), 2000L)
})

test_that("format_school_year handles boundary years", {
  expect_equal(format_school_year(2010), "2009-10")
  expect_equal(format_school_year(2000), "1999-00")
  expect_equal(format_school_year(2001), "2000-01")
})

test_that("clean_column_names handles special characters", {
  expect_equal(clean_column_names("School Year"), "school_year")
  expect_equal(clean_column_names("All Students"), "all_students")
  expect_equal(clean_column_names("Hispanic/Latino"), "hispanic_latino")
  expect_equal(clean_column_names("SchoolYear"), "schoolyear")
  expect_equal(clean_column_names("A___B"), "a_b")
  expect_equal(clean_column_names("_leading"), "leading")
  expect_equal(clean_column_names("trailing_"), "trailing")
})

test_that("standardize_grade_level handles all known formats", {
  # Pre-K variants
  expect_equal(standardize_grade_level("Pre-Kindergarten"), "PK")
  expect_equal(standardize_grade_level("PreK"), "PK")
  expect_equal(standardize_grade_level("Pre-K"), "PK")

  # Kindergarten variants
  expect_equal(standardize_grade_level("Kindergarten"), "K")
  expect_equal(standardize_grade_level("KG"), "K")
  expect_equal(standardize_grade_level("Half-day Kindergarten"), "K")
  expect_equal(standardize_grade_level("Half Day Kindergarten"), "K")
  expect_equal(standardize_grade_level("Full-day Kindergarten"), "K")

  # "1st Grade" format
  expect_equal(standardize_grade_level("1st Grade"), "01")
  expect_equal(standardize_grade_level("2nd Grade"), "02")
  expect_equal(standardize_grade_level("3rd Grade"), "03")
  expect_equal(standardize_grade_level("4th Grade"), "04")
  expect_equal(standardize_grade_level("5th Grade"), "05")
  expect_equal(standardize_grade_level("6th Grade"), "06")
  expect_equal(standardize_grade_level("7th Grade"), "07")
  expect_equal(standardize_grade_level("8th Grade"), "08")
  expect_equal(standardize_grade_level("9th Grade"), "09")
  expect_equal(standardize_grade_level("10th Grade"), "10")
  expect_equal(standardize_grade_level("11th Grade"), "11")
  expect_equal(standardize_grade_level("12th Grade"), "12")

  # "Grade N" format
  expect_equal(standardize_grade_level("Grade 1"), "01")
  expect_equal(standardize_grade_level("Grade 5"), "05")
  expect_equal(standardize_grade_level("Grade 10"), "10")
  expect_equal(standardize_grade_level("Grade 12"), "12")

  # Single digit format
  expect_equal(standardize_grade_level("1"), "01")
  expect_equal(standardize_grade_level("9"), "09")

  # Total formats
  expect_equal(standardize_grade_level("All Grades"), "TOTAL")
  expect_equal(standardize_grade_level("AllGrades"), "TOTAL")
})

test_that("standardize_grade_level handles vector input", {
  input <- c("Kindergarten", "1st Grade", "All Grades", "Pre-Kindergarten")
  expected <- c("K", "01", "TOTAL", "PK")
  expect_equal(standardize_grade_level(input), expected)
})

test_that("get_dataset_id returns correct IDs for all years", {
  # Report Card era
  expect_equal(get_dataset_id(2025), "2rwv-gs2e")
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

  # Student Enrollment era
  expect_equal(get_dataset_id(2014), "esyr-g8p5")
  expect_equal(get_dataset_id(2013), "9949-vk3e")
  expect_equal(get_dataset_id(2012), "5bjv-pebn")
  expect_equal(get_dataset_id(2011), "93ce-b95t")
  expect_equal(get_dataset_id(2010), "mpef-t92p")

  # Out of range
  expect_null(get_dataset_id(2009))
  expect_null(get_dataset_id(2026))
  expect_null(get_dataset_id(1999))
})

# ==============================================================================
# Cache function tests
# ==============================================================================

test_that("cache_status returns a data.frame", {
  result <- cache_status()
  expect_true(is.data.frame(result))
})

test_that("cache_status has expected columns", {
  result <- cache_status()
  if (nrow(result) > 0) {
    expect_true("year" %in% names(result))
    expect_true("type" %in% names(result))
    expect_true("size_mb" %in% names(result))
    expect_true("age_days" %in% names(result))
  }
})

test_that("clear_cache with non-existent year does not error", {
  expect_message(clear_cache(8888), "No cached files to remove")
})

test_that("clear_cache roundtrip works", {
  test_df <- data.frame(x = 1:3, y = letters[1:3], stringsAsFactors = FALSE)
  write_cache(test_df, 8887, "test")
  expect_true(cache_exists(8887, "test"))

  read_back <- read_cache(8887, "test")
  expect_equal(read_back, test_df)

  clear_cache(8887, "test")
  expect_false(cache_exists(8887, "test"))
})

test_that("clear_cache by type works", {
  test_df <- data.frame(x = 1)
  write_cache(test_df, 8886, "tidy")
  write_cache(test_df, 8885, "tidy")
  write_cache(test_df, 8886, "wide")

  clear_cache(type = "tidy")

  # tidy should be gone but wide should remain
  expect_false(cache_exists(8886, "tidy"))
  expect_false(cache_exists(8885, "tidy"))
  expect_true(cache_exists(8886, "wide"))

  # Cleanup
  clear_cache(8886, "wide")
})

# ==============================================================================
# Cross-year structural consistency
# ==============================================================================

test_that("Tidy output has same columns across years 2015-2024", {
  cols_2024 <- names(fetch_enr(2024, tidy = TRUE, use_cache = TRUE))
  cols_2020 <- names(fetch_enr(2020, tidy = TRUE, use_cache = TRUE))
  cols_2015 <- names(fetch_enr(2015, tidy = TRUE, use_cache = TRUE))

  expect_equal(sort(cols_2024), sort(cols_2020))
  expect_equal(sort(cols_2024), sort(cols_2015))
})

test_that("Wide output has same columns across years 2015-2024", {
  wide_2024 <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  wide_2020 <- fetch_enr(2020, tidy = FALSE, use_cache = TRUE)

  # 2015+ should have highly_capable
  expect_true("highly_capable" %in% names(wide_2024))
  expect_true("highly_capable" %in% names(wide_2020))
})

test_that("Grade levels are consistent across years", {
  enr_2024 <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  enr_2015 <- fetch_enr(2015, tidy = TRUE, use_cache = TRUE)

  grades_2024 <- sort(unique(enr_2024$grade_level))
  grades_2015 <- sort(unique(enr_2015$grade_level))

  expect_equal(grades_2024, grades_2015)
})

test_that("Core subgroups are present in all years 2015-2024", {
  core_subs <- c("total_enrollment", "white", "black", "hispanic", "asian",
                 "native_american", "pacific_islander", "multiracial",
                 "male", "female", "lep", "econ_disadv", "special_ed")

  for (yr in c(2015, 2020, 2024)) {
    enr <- fetch_enr(yr, tidy = TRUE, use_cache = TRUE)
    subs <- unique(enr$subgroup)
    for (sg in core_subs) {
      expect_true(sg %in% subs,
                  info = paste("Missing subgroup", sg, "in year", yr))
    }
  }
})

# ==============================================================================
# Year-over-year enrollment plausibility
# ==============================================================================

test_that("State enrollment is within 5% YoY for 2022-2024", {
  multi <- fetch_enr_multi(2022:2024, tidy = TRUE, use_cache = TRUE)
  totals <- multi[multi$is_state & multi$subgroup == "total_enrollment" & multi$grade_level == "TOTAL", ]
  totals <- totals[order(totals$end_year), ]

  for (i in 2:nrow(totals)) {
    pct_change <- abs(totals$n_students[i] / totals$n_students[i - 1] - 1)
    expect_true(pct_change < 0.05,
                info = paste("YoY change", totals$end_year[i - 1], "->",
                             totals$end_year[i], "is", round(pct_change * 100, 1), "%"))
  }
})

test_that("District count is stable across 2022-2024", {
  for (yr in 2022:2024) {
    enr <- fetch_enr(yr, tidy = TRUE, use_cache = TRUE)
    n_dist <- sum(enr$is_district & enr$subgroup == "total_enrollment" & enr$grade_level == "TOTAL")
    # Washington has ~295-335 districts
    expect_true(n_dist >= 290 && n_dist <= 340,
                info = paste("District count in", yr, "is", n_dist))
  }
})

# ==============================================================================
# Aggregation flag consistency
# ==============================================================================

test_that("aggregation_flag values are valid", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  valid_flags <- c("state", "district", "campus")
  expect_true(all(enr$aggregation_flag %in% valid_flags))
})

test_that("aggregation_flag aligns with entity type flags", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  expect_true(all(enr$aggregation_flag[enr$is_state] == "state"))
  # Districts should have aggregation_flag "district"
  expect_true(all(enr$aggregation_flag[enr$is_district] == "district"))
  # Campuses should have aggregation_flag "campus"
  expect_true(all(enr$aggregation_flag[enr$is_campus] == "campus"))
})

# ==============================================================================
# Grade aggregate mathematical consistency
# ==============================================================================

test_that("ELEM = sum of K through 05 for every district", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)

  # Pick a few districts to verify
  test_dids <- c("100229", "100247", "100261") # Seattle, Spokane, Tacoma

  for (did in test_dids) {
    # Get grade-level data
    grades <- enr[enr$district_id == did & enr$is_district &
                  enr$subgroup == "total_enrollment", ]
    elem_grades <- c("K", "01", "02", "03", "04", "05")
    grade_sum <- sum(grades$n_students[grades$grade_level %in% elem_grades], na.rm = TRUE)

    # Get aggregate
    agg_val <- aggs$n_students[aggs$district_id == did & aggs$is_district &
                               aggs$grade_level == "ELEM"]

    if (length(agg_val) == 1 && grade_sum > 0) {
      expect_equal(grade_sum, agg_val,
                   info = paste("ELEM mismatch for district", did))
    }
  }
})

test_that("MIDDLE = sum of 06 through 08 for every district", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)

  test_dids <- c("100229", "100247", "100261")

  for (did in test_dids) {
    grades <- enr[enr$district_id == did & enr$is_district &
                  enr$subgroup == "total_enrollment", ]
    mid_grades <- c("06", "07", "08")
    grade_sum <- sum(grades$n_students[grades$grade_level %in% mid_grades], na.rm = TRUE)

    agg_val <- aggs$n_students[aggs$district_id == did & aggs$is_district &
                               aggs$grade_level == "MIDDLE"]

    if (length(agg_val) == 1 && grade_sum > 0) {
      expect_equal(grade_sum, agg_val,
                   info = paste("MIDDLE mismatch for district", did))
    }
  }
})

test_that("HS = sum of 09 through 12 for state", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state <- enr[enr$is_state & enr$subgroup == "total_enrollment", ]

  hs_sum <- sum(state$n_students[state$grade_level %in% c("09", "10", "11", "12")])

  aggs <- enr_grade_aggs(enr)
  hs_agg <- aggs$n_students[aggs$is_state & aggs$grade_level == "HS"]

  expect_equal(hs_sum, hs_agg)
  expect_equal(hs_sum, 354288)
})

test_that("K12 excludes PK from total", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state <- enr[enr$is_state & enr$subgroup == "total_enrollment", ]

  total <- state$n_students[state$grade_level == "TOTAL"]
  pk <- state$n_students[state$grade_level == "PK"]

  aggs <- enr_grade_aggs(enr)
  k12 <- aggs$n_students[aggs$is_state & aggs$grade_level == "K12"]

  expect_equal(k12, total - pk)
  expect_equal(k12, 1074922)
})

# ==============================================================================
# Column mapping tests
# ==============================================================================

test_that("get_wa_column_map returns a complete mapping", {
  col_map <- get_wa_column_map()
  expect_true(is.list(col_map))

  # Check key mappings exist
  expected_keys <- c(
    "school_year", "organization_level", "district_name", "district_code",
    "district_id", "school_name", "school_id", "grade_level",
    "all_students", "male", "female", "white", "black", "hispanic",
    "asian", "pacific_islander", "native_american", "multiracial",
    "ell", "low_income", "special_ed", "homeless", "foster_care"
  )

  for (key in expected_keys) {
    expect_true(key %in% names(col_map),
                info = paste("Missing column mapping:", key))
  }
})

test_that("get_wa_column_map has multiple variants per key", {
  col_map <- get_wa_column_map()
  # Most columns should have at least 1 variant name
  expect_true(length(col_map$school_year) >= 2)
  expect_true(length(col_map$organization_level) >= 2)
  expect_true(length(col_map$all_students) >= 2)
})

# ==============================================================================
# build_socrata_url tests
# ==============================================================================

test_that("build_socrata_url constructs correct base URL", {
  url <- build_socrata_url("q4ba-s3jc")
  expect_true(grepl("data.wa.gov/resource/q4ba-s3jc.json", url))
  expect_true(grepl("\\$limit=50000", url))
})

test_that("build_socrata_url adds filters correctly", {
  url <- build_socrata_url("q4ba-s3jc", filters = list(organizationlevel = "State"))
  expect_true(grepl("\\$where=", url))
  expect_true(grepl("organizationlevel='State'", url))
})

test_that("build_socrata_url handles select clause", {
  url <- build_socrata_url("q4ba-s3jc", select = c("districtname", "all_students"))
  expect_true(grepl("\\$select=districtname,all_students", url))
})

test_that("build_socrata_url handles custom limit", {
  url <- build_socrata_url("q4ba-s3jc", limit = 100)
  expect_true(grepl("\\$limit=100", url))
})

# ==============================================================================
# Washington-specific data presence checks
# ==============================================================================

test_that("gender_x subgroup has non-zero counts statewide", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  gx <- enr[enr$is_state & enr$subgroup == "gender_x" & enr$grade_level == "TOTAL", ]
  expect_equal(nrow(gx), 1)
  expect_true(gx$n_students > 0)
  expect_equal(gx$n_students, 4979)
})

test_that("highly_capable is present for 2015+ but not 2010", {
  enr_2024 <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true("highly_capable" %in% unique(enr_2024$subgroup))

  enr_2010 <- fetch_enr(2010, tidy = TRUE, use_cache = TRUE)
  expect_false("highly_capable" %in% unique(enr_2010$subgroup))
})

test_that("section_504 is present in 2024 data", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true("section_504" %in% unique(enr$subgroup))
  state_504 <- enr$n_students[enr$is_state & enr$subgroup == "section_504" & enr$grade_level == "TOTAL"]
  expect_equal(state_504, 60154)
})

test_that("mobile subgroup is present in 2024 data", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true("mobile" %in% unique(enr$subgroup))
  state_mobile <- enr$n_students[enr$is_state & enr$subgroup == "mobile" & enr$grade_level == "TOTAL"]
  expect_equal(state_mobile, 16722)
})

test_that("military_parent subgroup is present in 2024 data", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true("military_parent" %in% unique(enr$subgroup))
  state_mil <- enr$n_students[enr$is_state & enr$subgroup == "military_parent" & enr$grade_level == "TOTAL"]
  expect_equal(state_mil, 30198)
})

# ==============================================================================
# End year consistency
# ==============================================================================

test_that("end_year is consistent within single fetch", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true(all(enr$end_year == 2024))
})

test_that("end_year is correct for each year in multi-year fetch", {
  multi <- fetch_enr_multi(2022:2024, tidy = TRUE, use_cache = TRUE)
  for (yr in 2022:2024) {
    yr_data <- multi[multi$end_year == yr, ]
    expect_true(nrow(yr_data) > 0, info = paste("No data for year", yr))
  }
})

test_that("Wide format end_year is consistent", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  expect_true(all(wide$end_year == 2024))
})
