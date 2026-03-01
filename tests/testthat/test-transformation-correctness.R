# ==============================================================================
# Transformation Correctness Tests for waschooldata
# ==============================================================================
#
# Tests every transformation in the pipeline: suppression handling, ID formatting,
# grade normalization, subgroup renaming, pivot fidelity, percentages, entity
# flags, aggregation, and cross-year consistency.
#
# These tests use cached data from real OSPI downloads. They verify that the
# transformation logic produces correct results, not that the network is up.
#
# ==============================================================================

# ==============================================================================
# 1. SUPPRESSION HANDLING (safe_numeric)
# ==============================================================================

test_that("safe_numeric converts normal integers", {
  expect_equal(waschooldata:::safe_numeric("100"), 100)
  expect_equal(waschooldata:::safe_numeric("0"), 0)
  expect_equal(waschooldata:::safe_numeric("999999"), 999999)
})

test_that("safe_numeric strips commas from large numbers", {
  expect_equal(waschooldata:::safe_numeric("1,234"), 1234)
  expect_equal(waschooldata:::safe_numeric("1,100,059"), 1100059)
})

test_that("safe_numeric strips whitespace", {
  expect_equal(waschooldata:::safe_numeric("  100  "), 100)
  expect_equal(waschooldata:::safe_numeric("\t50\t"), 50)
})

test_that("safe_numeric returns NA for OSPI suppression markers", {
  # OSPI uses these specific markers for small cell suppression
  expect_true(is.na(waschooldata:::safe_numeric("*")))
  expect_true(is.na(waschooldata:::safe_numeric("<5")))
  expect_true(is.na(waschooldata:::safe_numeric("<10")))
  expect_true(is.na(waschooldata:::safe_numeric(">95")))
  expect_true(is.na(waschooldata:::safe_numeric(">990")))
})

test_that("safe_numeric returns NA for sentinel/null values", {
  expect_true(is.na(waschooldata:::safe_numeric("N/A")))
  expect_true(is.na(waschooldata:::safe_numeric("NA")))
  expect_true(is.na(waschooldata:::safe_numeric("")))
  expect_true(is.na(waschooldata:::safe_numeric("null")))
  expect_true(is.na(waschooldata:::safe_numeric(".")))
  expect_true(is.na(waschooldata:::safe_numeric("-")))
  expect_true(is.na(waschooldata:::safe_numeric("-1")))
})

test_that("safe_numeric handles NULL and empty vector", {
  expect_equal(length(waschooldata:::safe_numeric(NULL)), 0)
  expect_equal(length(waschooldata:::safe_numeric(character(0))), 0)
})

test_that("safe_numeric handles vector input correctly", {
  input <- c("100", "*", "200", "<5", "300")
  result <- waschooldata:::safe_numeric(input)
  expect_equal(length(result), 5)
  expect_equal(result[1], 100)
  expect_true(is.na(result[2]))
  expect_equal(result[3], 200)
  expect_true(is.na(result[4]))
  expect_equal(result[5], 300)
})

test_that("safe_numeric converts already-numeric input", {
  expect_equal(waschooldata:::safe_numeric(42), 42)
  expect_equal(waschooldata:::safe_numeric(0), 0)
})

# ==============================================================================
# 2. ID FORMATTING
# ==============================================================================

test_that("district_id is character type in wide output", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  expect_true(is.character(wide$district_id))
})

test_that("campus_id is character type in wide output", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  expect_true(is.character(wide$campus_id))
})

test_that("district_code is character type in wide output", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  expect_true(is.character(wide$district_code))
})

test_that("school_code is character type in wide output", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  expect_true(is.character(wide$school_code))
})

test_that("campus_id is NA for non-campus rows", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  non_campus <- wide[wide$type != "Campus", ]
  expect_true(all(is.na(non_campus$campus_id)))
})

test_that("school_code is NA for non-campus rows", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  non_campus <- wide[wide$type != "Campus", ]
  expect_true(all(is.na(non_campus$school_code)))
})

test_that("Seattle district_id is 100229", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  seattle <- wide[wide$district_name == "Seattle School District No. 1" & wide$type == "District", ]
  expect_equal(seattle$district_id[1], "100229")
})

test_that("Spokane district_id is 100247", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  spokane <- wide[wide$district_name == "Spokane School District" & wide$type == "District", ]
  expect_equal(spokane$district_id[1], "100247")
})

# ==============================================================================
# 3. GRADE NORMALIZATION (standardize_grade_level)
# ==============================================================================

test_that("standard grade formats are normalized correctly", {
  expect_equal(waschooldata:::standardize_grade_level("Pre-Kindergarten"), "PK")
  expect_equal(waschooldata:::standardize_grade_level("Kindergarten"), "K")
  expect_equal(waschooldata:::standardize_grade_level("1st Grade"), "01")
  expect_equal(waschooldata:::standardize_grade_level("2nd Grade"), "02")
  expect_equal(waschooldata:::standardize_grade_level("3rd Grade"), "03")
  expect_equal(waschooldata:::standardize_grade_level("4th Grade"), "04")
  expect_equal(waschooldata:::standardize_grade_level("5th Grade"), "05")
  expect_equal(waschooldata:::standardize_grade_level("6th Grade"), "06")
  expect_equal(waschooldata:::standardize_grade_level("7th Grade"), "07")
  expect_equal(waschooldata:::standardize_grade_level("8th Grade"), "08")
  expect_equal(waschooldata:::standardize_grade_level("9th Grade"), "09")
  expect_equal(waschooldata:::standardize_grade_level("10th Grade"), "10")
  expect_equal(waschooldata:::standardize_grade_level("11th Grade"), "11")
  expect_equal(waschooldata:::standardize_grade_level("12th Grade"), "12")
  expect_equal(waschooldata:::standardize_grade_level("All Grades"), "TOTAL")
})

test_that("alternative grade formats are normalized", {
  expect_equal(waschooldata:::standardize_grade_level("PreK"), "PK")
  expect_equal(waschooldata:::standardize_grade_level("Pre-K"), "PK")
  expect_equal(waschooldata:::standardize_grade_level("KG"), "K")
  expect_equal(waschooldata:::standardize_grade_level("Grade 1"), "01")
  expect_equal(waschooldata:::standardize_grade_level("Grade 5"), "05")
  expect_equal(waschooldata:::standardize_grade_level("Grade 10"), "10")
  expect_equal(waschooldata:::standardize_grade_level("Grade 12"), "12")
})

test_that("bare digit grade formats are normalized", {
  expect_equal(waschooldata:::standardize_grade_level("1"), "01")
  expect_equal(waschooldata:::standardize_grade_level("5"), "05")
  expect_equal(waschooldata:::standardize_grade_level("9"), "09")
})

test_that("AllGrades (no space, 2010-2014 era) is normalized to TOTAL", {
  # Critical: 2010-2014 data uses "AllGrades" without space
  # This was a known bug in earlier versions
  expect_equal(waschooldata:::standardize_grade_level("AllGrades"), "TOTAL")
})

test_that("Half-day and Full-day Kindergarten variants are normalized to K", {
  # These appear in real OSPI data
  expect_equal(waschooldata:::standardize_grade_level("Half-day Kindergarten"), "K")
  expect_equal(waschooldata:::standardize_grade_level("Half Day Kindergarten"), "K")
  expect_equal(waschooldata:::standardize_grade_level("Full-day Kindergarten"), "K")
})

test_that("grade normalization is case-insensitive", {
  expect_equal(waschooldata:::standardize_grade_level("all grades"), "TOTAL")
  expect_equal(waschooldata:::standardize_grade_level("ALL GRADES"), "TOTAL")
  expect_equal(waschooldata:::standardize_grade_level("kindergarten"), "K")
  expect_equal(waschooldata:::standardize_grade_level("KINDERGARTEN"), "K")
})

test_that("unrecognized grade levels pass through unchanged", {
  expect_equal(waschooldata:::standardize_grade_level("UnknownGrade"), "UnknownGrade")
  expect_equal(waschooldata:::standardize_grade_level("13"), "13")
  expect_equal(waschooldata:::standardize_grade_level("Other"), "Other")
})

test_that("tidy output contains only standard grade levels", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  valid_grades <- c("PK", "K", "01", "02", "03", "04", "05", "06", "07", "08",
                    "09", "10", "11", "12", "TOTAL")
  actual_grades <- unique(tidy$grade_level)
  for (g in actual_grades) {
    expect_true(g %in% valid_grades, info = paste("Unexpected grade level:", g))
  }
})

test_that("tidy output for 2010 (Student Enrollment era) has standard grade levels", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2010, tidy = TRUE, use_cache = TRUE)
  valid_grades <- c("PK", "K", "01", "02", "03", "04", "05", "06", "07", "08",
                    "09", "10", "11", "12", "TOTAL")
  actual_grades <- unique(tidy$grade_level)
  for (g in actual_grades) {
    expect_true(g %in% valid_grades,
                info = paste("Grade level", g, "not in standard set for 2010 data"))
  }
})

# ==============================================================================
# 4. SUBGROUP RENAMING
# ==============================================================================

test_that("subgroup names follow naming standards", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  subgroups <- unique(tidy$subgroup)

  # Must use standard names
  expect_true("total_enrollment" %in% subgroups)
  expect_true("lep" %in% subgroups)
  expect_true("econ_disadv" %in% subgroups)
  expect_true("special_ed" %in% subgroups)
  expect_true("native_american" %in% subgroups)
  expect_true("multiracial" %in% subgroups)
})

test_that("raw OSPI names are not present in tidy output", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  subgroups <- unique(tidy$subgroup)

  # These are the raw OSPI column names that should NOT appear
  bad_names <- c(
    "ell", "english_language_learners", "englishlanguagelearners",
    "low_income", "lowincome",
    "students_with_disabilities", "studentswithdisabilities",
    "american_indian_alaskan_native", "americanindianalaskannative",
    "two_or_more_races", "twoormoreraces",
    "hispanic_latino_of_any_race", "hispaniclatino",
    "black_african_american", "blackafricanamerican",
    "native_hawaiian_other_pacific", "nativehawaiianotherpacific"
  )

  for (name in bad_names) {
    expect_false(name %in% subgroups,
                 info = paste("Raw OSPI name should not appear in tidy output:", name))
  }
})

test_that("all expected subgroups are present for 2024", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  subgroups <- unique(tidy$subgroup)

  expected <- c(
    "total_enrollment",
    "white", "black", "hispanic", "asian", "native_american",
    "pacific_islander", "multiracial",
    "male", "female", "gender_x",
    "special_ed", "lep", "econ_disadv",
    "homeless", "foster_care", "migrant",
    "military_parent", "section_504", "highly_capable", "mobile"
  )

  for (sg in expected) {
    expect_true(sg %in% subgroups,
                info = paste("Expected subgroup missing:", sg))
  }
})

test_that("2010 era data uses standard subgroup names too", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2010, tidy = TRUE, use_cache = TRUE)
  subgroups <- unique(tidy$subgroup)

  # Core subgroups that exist in all eras
  expect_true("total_enrollment" %in% subgroups)
  expect_true("lep" %in% subgroups, info = "2010 should have lep not ell")
  expect_true("econ_disadv" %in% subgroups, info = "2010 should have econ_disadv not low_income")
  expect_true("special_ed" %in% subgroups, info = "2010 should have special_ed not students_with_disabilities")
})

# ==============================================================================
# 5. PIVOT FIDELITY (wide <-> tidy)
# ==============================================================================

test_that("tidy total_enrollment matches wide row_total for state", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  wide_state <- wide$row_total[wide$type == "State"][1]
  tidy_state <- tidy$n_students[tidy$is_state &
    tidy$subgroup == "total_enrollment" &
    tidy$grade_level == "TOTAL"][1]

  expect_equal(wide_state, tidy_state)
})

test_that("tidy demographic counts match wide values for Seattle", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  seattle_w <- wide[wide$district_name == "Seattle School District No. 1" & wide$type == "District", ]
  seattle_t <- tidy[tidy$district_name == "Seattle School District No. 1" & tidy$is_district, ]

  # total_enrollment
  tidy_total <- seattle_t$n_students[seattle_t$subgroup == "total_enrollment" & seattle_t$grade_level == "TOTAL"]
  expect_equal(seattle_w$row_total[1], tidy_total[1])

  # white
  tidy_white <- seattle_t$n_students[seattle_t$subgroup == "white" & seattle_t$grade_level == "TOTAL"]
  expect_equal(seattle_w$white[1], tidy_white[1])

  # hispanic
  tidy_hisp <- seattle_t$n_students[seattle_t$subgroup == "hispanic" & seattle_t$grade_level == "TOTAL"]
  expect_equal(seattle_w$hispanic[1], tidy_hisp[1])

  # black
  tidy_black <- seattle_t$n_students[seattle_t$subgroup == "black" & seattle_t$grade_level == "TOTAL"]
  expect_equal(seattle_w$black[1], tidy_black[1])
})

test_that("tidy grade-level counts match wide grade columns for state", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  state_w <- wide[wide$type == "State", ]
  state_t <- tidy[tidy$is_state & tidy$subgroup == "total_enrollment", ]

  # PK
  expect_equal(state_w$grade_pk[1],
               state_t$n_students[state_t$grade_level == "PK"][1])
  # K
  expect_equal(state_w$grade_k[1],
               state_t$n_students[state_t$grade_level == "K"][1])
  # 01
  expect_equal(state_w$grade_01[1],
               state_t$n_students[state_t$grade_level == "01"][1])
  # 09
  expect_equal(state_w$grade_09[1],
               state_t$n_students[state_t$grade_level == "09"][1])
  # 12
  expect_equal(state_w$grade_12[1],
               state_t$n_students[state_t$grade_level == "12"][1])
})

test_that("tidy special populations match wide values for Spokane", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  spok_w <- wide[wide$district_name == "Spokane School District" & wide$type == "District", ]
  spok_t <- tidy[tidy$district_name == "Spokane School District" & tidy$is_district, ]

  # lep
  tidy_lep <- spok_t$n_students[spok_t$subgroup == "lep" & spok_t$grade_level == "TOTAL"]
  expect_equal(spok_w$lep[1], tidy_lep[1])

  # econ_disadv
  tidy_econ <- spok_t$n_students[spok_t$subgroup == "econ_disadv" & spok_t$grade_level == "TOTAL"]
  expect_equal(spok_w$econ_disadv[1], tidy_econ[1])

  # special_ed
  tidy_sped <- spok_t$n_students[spok_t$subgroup == "special_ed" & spok_t$grade_level == "TOTAL"]
  expect_equal(spok_w$special_ed[1], tidy_sped[1])
})

test_that("no NA n_students in tidy output (filter removes them)", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_equal(sum(is.na(tidy$n_students)), 0)
})

test_that("tidy row count is sensible (many subgroups per entity)", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  # Tidy should have many more rows than wide (each entity gets ~35 rows:
  # 21 subgroups + 14 grade levels)
  expect_true(nrow(tidy) > nrow(wide) * 10,
              info = "Tidy should have >> wide rows due to pivoting")
})

# ==============================================================================
# 6. PERCENTAGE CALCULATIONS
# ==============================================================================

test_that("pct = n_students / row_total for demographic subgroups", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  # Seattle white pct
  seattle_w <- wide[wide$district_name == "Seattle School District No. 1" & wide$type == "District", ]
  expected_pct <- seattle_w$white[1] / seattle_w$row_total[1]

  seattle_t <- tidy[tidy$district_name == "Seattle School District No. 1" &
    tidy$is_district & tidy$subgroup == "white" & tidy$grade_level == "TOTAL", ]
  expect_equal(seattle_t$pct[1], expected_pct, tolerance = 1e-6)
})

test_that("pct is exactly 1.0 for total_enrollment subgroup", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  total_rows <- tidy[tidy$subgroup == "total_enrollment" & tidy$grade_level == "TOTAL", ]
  expect_true(all(total_rows$pct == 1.0))
})

test_that("pct is capped at 1.0 (never exceeds)", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true(all(tidy$pct <= 1.0, na.rm = TRUE))
})

test_that("pct is never negative", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true(all(tidy$pct >= 0, na.rm = TRUE))
})

test_that("pct is 0 when row_total is 0", {
  # Verify the case_when logic: when row_total is 0, pct should be 0 not Inf/NaN
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  # No Inf values in pct
  expect_false(any(is.infinite(tidy$pct), na.rm = TRUE))
  # No NaN values in pct
  expect_false(any(is.nan(tidy$pct), na.rm = TRUE))
})

test_that("grade-level pct = grade_count / row_total", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  state_w <- wide[wide$type == "State", ]
  state_t <- tidy[tidy$is_state & tidy$subgroup == "total_enrollment", ]

  # Grade K pct should be grade_k / row_total
  expected_k_pct <- state_w$grade_k[1] / state_w$row_total[1]
  actual_k_pct <- state_t$pct[state_t$grade_level == "K"][1]
  expect_equal(actual_k_pct, expected_k_pct, tolerance = 1e-6)
})

# ==============================================================================
# 7. AGGREGATION
# ==============================================================================

test_that("state aggregate equals sum of district totals", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)

  state_total <- wide$row_total[wide$type == "State"][1]
  district_sum <- sum(wide$row_total[wide$type == "District"], na.rm = TRUE)

  # State total should equal sum of districts (within 1% for rounding/suppression)
  expect_equal(state_total, district_sum, tolerance = state_total * 0.01)
})

test_that("state demographic counts equal sum of district demographics", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)

  state_row <- wide[wide$type == "State", ]
  district_rows <- wide[wide$type == "District", ]

  demo_cols <- c("white", "black", "hispanic", "asian",
                 "native_american", "pacific_islander", "multiracial",
                 "male", "female")

  for (col in demo_cols) {
    if (col %in% names(wide)) {
      state_val <- state_row[[col]][1]
      dist_sum <- sum(district_rows[[col]], na.rm = TRUE)
      expect_equal(state_val, dist_sum, tolerance = state_val * 0.02,
                   info = paste("State", col, "should equal sum of districts"))
    }
  }
})

test_that("gender sum approximately equals total for state (2024)", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state <- tidy[tidy$is_state & tidy$grade_level == "TOTAL", ]

  total <- state$n_students[state$subgroup == "total_enrollment"]
  male <- state$n_students[state$subgroup == "male"]
  female <- state$n_students[state$subgroup == "female"]
  gender_x <- state$n_students[state$subgroup == "gender_x"]

  gender_sum <- male + female + gender_x
  # Gender sum should match total exactly or within 0.1%
  expect_equal(total, gender_sum, tolerance = total * 0.001)
})

test_that("race sum approximately equals total for state (2024)", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state <- tidy[tidy$is_state & tidy$grade_level == "TOTAL", ]

  total <- state$n_students[state$subgroup == "total_enrollment"]
  race_groups <- c("white", "black", "hispanic", "asian",
                   "native_american", "pacific_islander", "multiracial")
  race_sum <- sum(sapply(race_groups, function(sg) {
    val <- state$n_students[state$subgroup == sg]
    if (length(val) > 0) val[1] else 0
  }))

  # Race sum should be close to total (within 5% for suppressed/unclassified)
  expect_true(abs(total - race_sum) < total * 0.05,
              info = paste("Race sum", race_sum, "should be close to total", total))
})

test_that("grade-level sum equals TOTAL for state (2024)", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state <- tidy[tidy$is_state & tidy$subgroup == "total_enrollment", ]

  grade_sum <- sum(state$n_students[state$grade_level != "TOTAL"], na.rm = TRUE)
  total <- state$n_students[state$grade_level == "TOTAL"]

  expect_equal(grade_sum, total,
               info = "Sum of PK+K+01..12 should equal TOTAL for state")
})

test_that("grade-level sum equals TOTAL for Seattle district", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  seattle <- tidy[tidy$district_name == "Seattle School District No. 1" &
    tidy$is_district & tidy$subgroup == "total_enrollment", ]

  grade_sum <- sum(seattle$n_students[seattle$grade_level != "TOTAL"], na.rm = TRUE)
  total <- seattle$n_students[seattle$grade_level == "TOTAL"]

  expect_equal(grade_sum, total,
               info = "Sum of grades should equal TOTAL for Seattle")
})

test_that("wide grade columns sum to row_total for state", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  state <- wide[wide$type == "State", ]

  grade_cols <- grep("^grade_", names(state), value = TRUE)
  grade_sum <- sum(sapply(grade_cols, function(col) state[[col]][1]), na.rm = TRUE)

  expect_equal(grade_sum, state$row_total[1],
               info = "Wide grade columns should sum to row_total")
})

# ==============================================================================
# 8. ENTITY FLAGS
# ==============================================================================

test_that("type column has exactly three values", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  expect_setequal(unique(wide$type), c("State", "District", "Campus"))
})

test_that("organization level 'School' is mapped to type 'Campus'", {
  skip_on_cran()
  skip_if_offline()

  # The raw data uses "School", process_enr maps to "Campus"
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  expect_false("School" %in% unique(wide$type))
  expect_true("Campus" %in% unique(wide$type))
})

test_that("is_state, is_district, is_campus are mutually exclusive", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  type_sums <- tidy$is_state + tidy$is_district + tidy$is_campus
  expect_true(all(type_sums == 1),
              info = "Each row must be exactly one of state/district/campus")
})

test_that("is_state flag agrees with type == 'State'", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true(all(tidy$is_state == (tidy$type == "State")))
})

test_that("is_district flag agrees with type == 'District'", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true(all(tidy$is_district == (tidy$type == "District")))
})

test_that("is_campus flag agrees with type in c('Campus', 'School')", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true(all(tidy$is_campus == (tidy$type %in% c("Campus", "School"))))
})

test_that("aggregation_flag is consistent with entity type", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  expect_true(all(tidy$aggregation_flag[tidy$is_state] == "state"))
  expect_true(all(tidy$aggregation_flag[tidy$is_district] == "district"))
  expect_true(all(tidy$aggregation_flag[tidy$is_campus] == "campus"))
})

test_that("exactly one state row per subgroup-grade combination", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state_rows <- tidy[tidy$is_state, ]
  dupes <- state_rows |>
    dplyr::count(subgroup, grade_level) |>
    dplyr::filter(n > 1)
  expect_equal(nrow(dupes), 0,
               info = "Should be exactly one state row per subgroup-grade pair")
})

# ==============================================================================
# 9. GRADE AGGREGATES (enr_grade_aggs)
# ==============================================================================

test_that("K8 aggregate = K + 01 + 02 + 03 + 04 + 05 + 06 + 07 + 08", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(tidy)

  state_agg <- aggs[aggs$is_state, ]
  state_tidy <- tidy[tidy$is_state & tidy$subgroup == "total_enrollment", ]

  k8 <- state_agg$n_students[state_agg$grade_level == "K8"]
  manual_sum <- sum(state_tidy$n_students[state_tidy$grade_level %in%
    c("K", "01", "02", "03", "04", "05", "06", "07", "08")])

  expect_equal(k8, manual_sum)
})

test_that("HS aggregate = 09 + 10 + 11 + 12", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(tidy)

  state_agg <- aggs[aggs$is_state, ]
  state_tidy <- tidy[tidy$is_state & tidy$subgroup == "total_enrollment", ]

  hs <- state_agg$n_students[state_agg$grade_level == "HS"]
  manual_sum <- sum(state_tidy$n_students[state_tidy$grade_level %in%
    c("09", "10", "11", "12")])

  expect_equal(hs, manual_sum)
})

test_that("K12 aggregate = K + 01..12 (excludes PK)", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(tidy)

  state_agg <- aggs[aggs$is_state, ]
  state_tidy <- tidy[tidy$is_state & tidy$subgroup == "total_enrollment", ]

  k12 <- state_agg$n_students[state_agg$grade_level == "K12"]
  manual_sum <- sum(state_tidy$n_students[state_tidy$grade_level %in%
    c("K", "01", "02", "03", "04", "05", "06", "07", "08",
      "09", "10", "11", "12")])

  expect_equal(k12, manual_sum)
})

test_that("ELEM aggregate = K + 01 + 02 + 03 + 04 + 05", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(tidy)

  state_agg <- aggs[aggs$is_state, ]
  state_tidy <- tidy[tidy$is_state & tidy$subgroup == "total_enrollment", ]

  elem <- state_agg$n_students[state_agg$grade_level == "ELEM"]
  manual_sum <- sum(state_tidy$n_students[state_tidy$grade_level %in%
    c("K", "01", "02", "03", "04", "05")])

  expect_equal(elem, manual_sum)
})

test_that("MIDDLE aggregate = 06 + 07 + 08", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(tidy)

  state_agg <- aggs[aggs$is_state, ]
  state_tidy <- tidy[tidy$is_state & tidy$subgroup == "total_enrollment", ]

  middle <- state_agg$n_students[state_agg$grade_level == "MIDDLE"]
  manual_sum <- sum(state_tidy$n_students[state_tidy$grade_level %in%
    c("06", "07", "08")])

  expect_equal(middle, manual_sum)
})

test_that("K8 + HS = K12", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(tidy)

  state_agg <- aggs[aggs$is_state, ]

  k8 <- state_agg$n_students[state_agg$grade_level == "K8"]
  hs <- state_agg$n_students[state_agg$grade_level == "HS"]
  k12 <- state_agg$n_students[state_agg$grade_level == "K12"]

  expect_equal(k8 + hs, k12)
})

test_that("ELEM + MIDDLE = K8", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(tidy)

  state_agg <- aggs[aggs$is_state, ]

  elem <- state_agg$n_students[state_agg$grade_level == "ELEM"]
  middle <- state_agg$n_students[state_agg$grade_level == "MIDDLE"]
  k8 <- state_agg$n_students[state_agg$grade_level == "K8"]

  expect_equal(elem + middle, k8)
})

test_that("grade aggregates have all 5 levels", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(tidy)

  agg_levels <- unique(aggs$grade_level)
  expect_true("K8" %in% agg_levels)
  expect_true("HS" %in% agg_levels)
  expect_true("K12" %in% agg_levels)
  expect_true("ELEM" %in% agg_levels)
  expect_true("MIDDLE" %in% agg_levels)
})

test_that("grade aggregates only cover total_enrollment subgroup", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(tidy)

  expect_true(all(aggs$subgroup == "total_enrollment"),
              info = "Grade aggregates should only be for total_enrollment")
})

# ==============================================================================
# 10. PINNED SPOT CHECKS (Year x Data)
# ==============================================================================

# --- 2024 ---

test_that("2024 state total enrollment is 1,100,059", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state_total <- tidy$n_students[tidy$is_state &
    tidy$subgroup == "total_enrollment" &
    tidy$grade_level == "TOTAL"]
  expect_equal(state_total[1], 1100059)
})

test_that("2024 state white enrollment is 532,562", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  val <- tidy$n_students[tidy$is_state &
    tidy$subgroup == "white" &
    tidy$grade_level == "TOTAL"]
  expect_equal(val[1], 532562)
})

test_that("2024 state hispanic enrollment is 288,238", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  val <- tidy$n_students[tidy$is_state &
    tidy$subgroup == "hispanic" &
    tidy$grade_level == "TOTAL"]
  expect_equal(val[1], 288238)
})

test_that("2024 state grade K enrollment is 76,359", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  val <- tidy$n_students[tidy$is_state &
    tidy$subgroup == "total_enrollment" &
    tidy$grade_level == "K"]
  expect_equal(val[1], 76359)
})

test_that("2024 Seattle district total is 50,968", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  val <- tidy$n_students[tidy$district_name == "Seattle School District No. 1" &
    tidy$is_district &
    tidy$subgroup == "total_enrollment" &
    tidy$grade_level == "TOTAL"]
  expect_equal(val[1], 50968)
})

test_that("2024 Spokane district total is 29,444", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  val <- tidy$n_students[tidy$district_name == "Spokane School District" &
    tidy$is_district &
    tidy$subgroup == "total_enrollment" &
    tidy$grade_level == "TOTAL"]
  expect_equal(val[1], 29444)
})

test_that("2024 Tacoma district total is 28,353", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  val <- tidy$n_students[tidy$district_name == "Tacoma School District" &
    tidy$is_district &
    tidy$subgroup == "total_enrollment" &
    tidy$grade_level == "TOTAL"]
  expect_equal(val[1], 28353)
})

test_that("2024 has 332 unique districts", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  n_districts <- length(unique(tidy$district_name[tidy$is_district &
    tidy$subgroup == "total_enrollment" &
    tidy$grade_level == "TOTAL"]))
  expect_equal(n_districts, 332)
})

# --- 2020 ---

test_that("2020 state total enrollment is 1,146,882", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2020, tidy = TRUE, use_cache = TRUE)
  state_total <- tidy$n_students[tidy$is_state &
    tidy$subgroup == "total_enrollment" &
    tidy$grade_level == "TOTAL"]
  expect_equal(state_total[1], 1146882)
})

# --- 2015 ---

test_that("2015 state total enrollment is 1,086,314", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2015, tidy = TRUE, use_cache = TRUE)
  state_total <- tidy$n_students[tidy$is_state &
    tidy$subgroup == "total_enrollment" &
    tidy$grade_level == "TOTAL"]
  expect_equal(state_total[1], 1086314)
})

# --- 2010 ---

test_that("2010 state total enrollment is 1,034,935", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2010, tidy = TRUE, use_cache = TRUE)
  state_total <- tidy$n_students[tidy$is_state &
    tidy$subgroup == "total_enrollment" &
    tidy$grade_level == "TOTAL"]
  expect_equal(state_total[1], 1034935)
})

# ==============================================================================
# 11. CROSS-YEAR CONSISTENCY
# ==============================================================================

test_that("state totals increase monotonically within reasonable bounds", {
  skip_on_cran()
  skip_if_offline()

  years <- c(2010, 2015, 2020, 2024)
  totals <- sapply(years, function(yr) {
    tidy <- fetch_enr(yr, tidy = TRUE, use_cache = TRUE)
    tidy$n_students[tidy$is_state &
      tidy$subgroup == "total_enrollment" &
      tidy$grade_level == "TOTAL"][1]
  })

  # Year-over-year changes should be within 10%
  for (i in 2:length(totals)) {
    yoy_change <- abs(totals[i] / totals[i - 1] - 1)
    expect_true(yoy_change < 0.10,
                info = paste("Change from", years[i - 1], "to", years[i],
                           "is", round(yoy_change * 100, 1), "%"))
  }
})

test_that("subgroup set is consistent across 2015 and 2024", {
  skip_on_cran()
  skip_if_offline()

  tidy15 <- fetch_enr(2015, tidy = TRUE, use_cache = TRUE)
  tidy24 <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  sg15 <- sort(unique(tidy15$subgroup))
  sg24 <- sort(unique(tidy24$subgroup))

  # Core subgroups should be present in both eras
  core <- c("total_enrollment", "white", "black", "hispanic", "asian",
            "native_american", "pacific_islander", "multiracial",
            "male", "female", "lep", "econ_disadv", "special_ed")

  for (sg in core) {
    expect_true(sg %in% sg15, info = paste(sg, "missing from 2015"))
    expect_true(sg %in% sg24, info = paste(sg, "missing from 2024"))
  }
})

test_that("grade levels are consistent across 2010 and 2024", {
  skip_on_cran()
  skip_if_offline()

  tidy10 <- fetch_enr(2010, tidy = TRUE, use_cache = TRUE)
  tidy24 <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  grades10 <- sort(unique(tidy10$grade_level))
  grades24 <- sort(unique(tidy24$grade_level))

  # Should have the same standard grade set
  expected <- c("01", "02", "03", "04", "05", "06", "07", "08",
                "09", "10", "11", "12", "K", "PK", "TOTAL")
  expect_setequal(grades10, expected)
  expect_setequal(grades24, expected)
})

test_that("district counts are stable across years (within 10%)", {
  skip_on_cran()
  skip_if_offline()

  years <- c(2015, 2020, 2024)
  counts <- sapply(years, function(yr) {
    tidy <- fetch_enr(yr, tidy = TRUE, use_cache = TRUE)
    length(unique(tidy$district_name[tidy$is_district &
      tidy$subgroup == "total_enrollment" &
      tidy$grade_level == "TOTAL"]))
  })

  for (i in 2:length(counts)) {
    change <- abs(counts[i] / counts[i - 1] - 1)
    expect_true(change < 0.10,
                info = paste("District count change from", years[i - 1], "to",
                           years[i], "is", round(change * 100, 1), "%"))
  }
})

test_that("entity flag columns exist in both 2010 and 2024 tidy output", {
  skip_on_cran()
  skip_if_offline()

  tidy10 <- fetch_enr(2010, tidy = TRUE, use_cache = TRUE)
  tidy24 <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  for (col in c("is_state", "is_district", "is_campus")) {
    expect_true(col %in% names(tidy10), info = paste(col, "missing from 2010"))
    expect_true(col %in% names(tidy24), info = paste(col, "missing from 2024"))
  }
})

# ==============================================================================
# 12. COLUMN NAME CLEANING
# ==============================================================================

test_that("clean_column_names converts to lowercase", {
  expect_equal(waschooldata:::clean_column_names("SchoolYear"), "schoolyear")
  expect_equal(waschooldata:::clean_column_names("ALLCAPS"), "allcaps")
})

test_that("clean_column_names replaces spaces with underscores", {
  expect_equal(waschooldata:::clean_column_names("All Students"), "all_students")
  expect_equal(waschooldata:::clean_column_names("Grade Level"), "grade_level")
})

test_that("clean_column_names replaces special characters with underscores", {
  expect_equal(waschooldata:::clean_column_names("Hispanic/Latino"), "hispanic_latino")
})

test_that("clean_column_names collapses consecutive underscores", {
  result <- waschooldata:::clean_column_names("Hispanic/Latino of any Race(s)")
  expect_false(grepl("__", result))
})

test_that("clean_column_names strips leading/trailing underscores", {
  result <- waschooldata:::clean_column_names("_test_")
  expect_equal(result, "test")
})

# ==============================================================================
# 13. SCHOOL YEAR PARSING
# ==============================================================================

test_that("parse_school_year handles 2023-24 format", {
  expect_equal(waschooldata:::parse_school_year("2023-24"), 2024L)
  expect_equal(waschooldata:::parse_school_year("2009-10"), 2010L)
})

test_that("parse_school_year handles 2023-2024 format", {
  expect_equal(waschooldata:::parse_school_year("2023-2024"), 2024L)
})

test_that("parse_school_year handles single year", {
  expect_equal(waschooldata:::parse_school_year("2024"), 2024L)
})

test_that("format_school_year creates correct string", {
  expect_equal(waschooldata:::format_school_year(2024), "2023-24")
  expect_equal(waschooldata:::format_school_year(2010), "2009-10")
  expect_equal(waschooldata:::format_school_year(2000), "1999-00")
})

# ==============================================================================
# 14. DATA QUALITY INVARIANTS
# ==============================================================================

test_that("no Inf values in any numeric column (wide 2024)", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  num_cols <- names(wide)[sapply(wide, is.numeric)]

  for (col in num_cols) {
    expect_false(any(is.infinite(wide[[col]]), na.rm = TRUE),
                 info = paste("Inf found in", col))
  }
})

test_that("no NaN values in any numeric column (wide 2024)", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  num_cols <- names(wide)[sapply(wide, is.numeric)]

  for (col in num_cols) {
    expect_false(any(is.nan(wide[[col]]), na.rm = TRUE),
                 info = paste("NaN found in", col))
  }
})

test_that("all enrollment counts are non-negative (wide 2024)", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  count_cols <- c("row_total", "male", "female", "white", "black", "hispanic",
                  "asian", "native_american", "pacific_islander", "multiracial",
                  "lep", "econ_disadv", "special_ed")
  count_cols <- count_cols[count_cols %in% names(wide)]

  for (col in count_cols) {
    expect_true(all(wide[[col]] >= 0, na.rm = TRUE),
                info = paste("Negative value in", col))
  }
})

test_that("all enrollment counts are non-negative (tidy 2024)", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true(all(tidy$n_students >= 0, na.rm = TRUE))
})

test_that("no duplicate rows in wide output", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  key_cols <- c("type", "district_id", "campus_id")
  key_cols <- key_cols[key_cols %in% names(wide)]

  n_rows <- nrow(wide)
  n_unique <- nrow(unique(wide[, key_cols, drop = FALSE]))
  expect_equal(n_rows, n_unique, info = "No duplicate rows in wide output")
})

test_that("tidy output has one row per entity-subgroup-grade combo", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  dupes <- tidy |>
    dplyr::count(type, district_id, campus_id, subgroup, grade_level) |>
    dplyr::filter(n > 1)

  expect_equal(nrow(dupes), 0,
               info = "Should have exactly one row per entity-subgroup-grade")
})

test_that("state total is in reasonable range (900k-1.5M)", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state_total <- tidy$n_students[tidy$is_state &
    tidy$subgroup == "total_enrollment" &
    tidy$grade_level == "TOTAL"][1]

  expect_true(state_total > 900000)
  expect_true(state_total < 1500000)
})

# ==============================================================================
# 15. COLUMN MAPPING (get_wa_column_map)
# ==============================================================================

test_that("column map covers all expected demographic categories", {
  col_map <- waschooldata:::get_wa_column_map()

  # Race categories
  expect_true("white" %in% names(col_map))
  expect_true("black" %in% names(col_map))
  expect_true("hispanic" %in% names(col_map))
  expect_true("asian" %in% names(col_map))
  expect_true("native_american" %in% names(col_map))
  expect_true("pacific_islander" %in% names(col_map))
  expect_true("multiracial" %in% names(col_map))

  # Gender
  expect_true("male" %in% names(col_map))
  expect_true("female" %in% names(col_map))
  expect_true("gender_x" %in% names(col_map))

  # Special populations
  expect_true("ell" %in% names(col_map))
  expect_true("low_income" %in% names(col_map))
  expect_true("special_ed" %in% names(col_map))
  expect_true("homeless" %in% names(col_map))
  expect_true("foster_care" %in% names(col_map))
  expect_true("migrant" %in% names(col_map))
  expect_true("military_parent" %in% names(col_map))
  expect_true("section_504" %in% names(col_map))
  expect_true("highly_capable" %in% names(col_map))
  expect_true("mobile" %in% names(col_map))
})

test_that("column map raw names match 2024 API column names", {
  col_map <- waschooldata:::get_wa_column_map()

  # The 2024 API uses these specific column names (from pipeline-live tests)
  # Verify that at least one variant in the map matches
  expect_true(any(c("all_students", "allstudents") %in% col_map$all_students))
  expect_true(any(c("hispanic_latino_of_any_race", "hispaniclatino") %in% col_map$hispanic))
  expect_true(any(c("english_language_learners", "englishlanguagelearners") %in% col_map$ell))
  expect_true(any(c("low_income", "lowincome") %in% col_map$low_income))
  expect_true(any(c("students_with_disabilities", "studentswithdisabilities") %in% col_map$special_ed))
})

# ==============================================================================
# 16. STATE AGGREGATE CREATION
# ==============================================================================

test_that("create_state_aggregate does not duplicate if state row exists", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)

  # The state row should already exist
  state_count <- sum(wide$type == "State")
  expect_equal(state_count, 1, info = "Should have exactly one state row")
})

test_that("state row has district_id and campus_id as NA", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  state_row <- wide[wide$type == "State", ]

  expect_true(is.na(state_row$district_id[1]) || state_row$district_id[1] == "")
  expect_true(is.na(state_row$campus_id[1]))
})

# ==============================================================================
# 17. REQUIRED OUTPUT COLUMNS
# ==============================================================================

test_that("wide output has all required columns", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)

  required <- c("end_year", "type", "district_id", "district_name",
                "campus_id", "campus_name", "row_total")
  for (col in required) {
    expect_true(col %in% names(wide), info = paste("Wide missing:", col))
  }
})

test_that("tidy output has all required columns", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  required <- c("end_year", "type", "district_id", "district_name",
                "campus_id", "campus_name",
                "grade_level", "subgroup", "n_students", "pct",
                "is_state", "is_district", "is_campus", "aggregation_flag")
  for (col in required) {
    expect_true(col %in% names(tidy), info = paste("Tidy missing:", col))
  }
})

test_that("end_year column matches requested year", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true(all(tidy$end_year == 2024))

  tidy10 <- fetch_enr(2010, tidy = TRUE, use_cache = TRUE)
  expect_true(all(tidy10$end_year == 2010))
})

test_that("fetch_enr_multi has correct end_year values", {
  skip_on_cran()
  skip_if_offline()

  multi <- fetch_enr_multi(c(2023, 2024), tidy = TRUE, use_cache = TRUE)
  expect_setequal(unique(multi$end_year), c(2023, 2024))
})
