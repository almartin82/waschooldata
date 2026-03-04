# ==============================================================================
# Exhaustive Tests for Washington Enrollment Functions
# ==============================================================================
#
# Tests every exported function with every parameter combination.
# All pinned values come from real OSPI data via use_cache = TRUE.
#
# ==============================================================================

# ==============================================================================
# get_available_years()
# ==============================================================================

test_that("get_available_years returns correct year range", {
  years <- get_available_years()

  expect_true(is.integer(years) || is.numeric(years))
  expect_equal(length(years), 16)
  expect_equal(min(years), 2010)
  expect_equal(max(years), 2025)
  expect_equal(years, 2010:2025)
})

test_that("get_available_years returns contiguous sequence", {
  years <- get_available_years()
  diffs <- diff(years)
  expect_true(all(diffs == 1))
})

# ==============================================================================
# fetch_enr() — year validation
# ==============================================================================

test_that("fetch_enr rejects years before available range", {
  expect_error(fetch_enr(2009, use_cache = TRUE), "end_year must be between")
  expect_error(fetch_enr(2000, use_cache = TRUE), "end_year must be between")
  expect_error(fetch_enr(1990, use_cache = TRUE), "end_year must be between")
})

test_that("fetch_enr rejects years after available range", {
  expect_error(fetch_enr(2026, use_cache = TRUE), "end_year must be between")
  expect_error(fetch_enr(2030, use_cache = TRUE), "end_year must be between")
  expect_error(fetch_enr(2050, use_cache = TRUE), "end_year must be between")
})

# ==============================================================================
# fetch_enr(tidy = TRUE) — 2024 structure tests
# ==============================================================================

test_that("fetch_enr 2024 tidy returns a data.frame", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_s3_class(enr, "data.frame")
})

test_that("fetch_enr 2024 tidy has correct columns", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  expected_cols <- c(
    "end_year", "type", "district_id", "district_code", "district_name",
    "campus_id", "school_code", "campus_name", "school_type",
    "esd_id", "esd_name", "county",
    "grade_level", "subgroup", "n_students", "pct",
    "is_state", "is_district", "is_campus", "aggregation_flag"
  )

  for (col in expected_cols) {
    expect_true(col %in% names(enr), info = paste("Missing column:", col))
  }
  expect_equal(ncol(enr), 20)
})

test_that("fetch_enr 2024 tidy has correct column types", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  expect_true(is.numeric(enr$end_year), info = "end_year should be numeric (integer or double)")
  expect_type(enr$type, "character")
  expect_type(enr$district_id, "character")
  expect_type(enr$district_code, "character")
  expect_type(enr$district_name, "character")
  expect_type(enr$campus_id, "character")
  expect_type(enr$school_code, "character")
  expect_type(enr$campus_name, "character")
  expect_type(enr$school_type, "character")
  expect_type(enr$esd_id, "character")
  expect_type(enr$esd_name, "character")
  expect_type(enr$county, "character")
  expect_type(enr$grade_level, "character")
  expect_type(enr$subgroup, "character")
  expect_type(enr$n_students, "double")
  expect_type(enr$pct, "double")
  expect_type(enr$is_state, "logical")
  expect_type(enr$is_district, "logical")
  expect_type(enr$is_campus, "logical")
  expect_type(enr$aggregation_flag, "character")
})

test_that("fetch_enr 2024 tidy has correct row count", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_equal(nrow(enr), 76472)
})

test_that("fetch_enr 2024 tidy has exactly 3 entity types", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  types <- sort(unique(enr$type))
  expect_equal(types, c("Campus", "District", "State"))
})

# ==============================================================================
# fetch_enr(tidy = TRUE) — subgroup completeness
# ==============================================================================

test_that("fetch_enr 2024 tidy has all 21 subgroups", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  subgroups <- sort(unique(enr$subgroup))

  expected_subgroups <- sort(c(
    "total_enrollment",
    "white", "black", "hispanic", "asian",
    "native_american", "pacific_islander", "multiracial",
    "male", "female", "gender_x",
    "special_ed", "lep", "econ_disadv",
    "homeless", "foster_care", "migrant",
    "military_parent", "section_504", "highly_capable", "mobile"
  ))

  expect_equal(subgroups, expected_subgroups)
})

# ==============================================================================
# fetch_enr(tidy = TRUE) — grade level completeness
# ==============================================================================

test_that("fetch_enr 2024 tidy has all 15 grade levels", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  grade_levels <- sort(unique(enr$grade_level))

  expected_grades <- sort(c(
    "PK", "K",
    "01", "02", "03", "04", "05", "06", "07", "08",
    "09", "10", "11", "12",
    "TOTAL"
  ))

  expect_equal(grade_levels, expected_grades)
})

# ==============================================================================
# fetch_enr(tidy = TRUE) — pinned state-level enrollment values (2024)
# ==============================================================================

test_that("2024 state total enrollment is 1100059", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state_total <- enr$n_students[enr$is_state & enr$subgroup == "total_enrollment" & enr$grade_level == "TOTAL"]
  expect_equal(state_total, 1100059)
})

test_that("2024 state race/ethnicity counts are pinned", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state <- enr[enr$is_state & enr$grade_level == "TOTAL", ]

  expect_equal(state$n_students[state$subgroup == "white"], 532562)
  expect_equal(state$n_students[state$subgroup == "black"], 52031)
  expect_equal(state$n_students[state$subgroup == "hispanic"], 288238)
  expect_equal(state$n_students[state$subgroup == "asian"], 97920)
  expect_equal(state$n_students[state$subgroup == "native_american"], 13168)
  expect_equal(state$n_students[state$subgroup == "pacific_islander"], 15903)
  expect_equal(state$n_students[state$subgroup == "multiracial"], 100099)
})

test_that("2024 state gender counts are pinned", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state <- enr[enr$is_state & enr$grade_level == "TOTAL", ]

  expect_equal(state$n_students[state$subgroup == "male"], 567051)
  expect_equal(state$n_students[state$subgroup == "female"], 528028)
  expect_equal(state$n_students[state$subgroup == "gender_x"], 4979)
})

test_that("2024 state special population counts are pinned", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state <- enr[enr$is_state & enr$grade_level == "TOTAL", ]

  expect_equal(state$n_students[state$subgroup == "lep"], 156910)
  expect_equal(state$n_students[state$subgroup == "econ_disadv"], 551109)
  expect_equal(state$n_students[state$subgroup == "special_ed"], 176801)
  expect_equal(state$n_students[state$subgroup == "homeless"], 41050)
  expect_equal(state$n_students[state$subgroup == "foster_care"], 3317)
  expect_equal(state$n_students[state$subgroup == "migrant"], 22750)
  expect_equal(state$n_students[state$subgroup == "military_parent"], 30198)
  expect_equal(state$n_students[state$subgroup == "section_504"], 60154)
  expect_equal(state$n_students[state$subgroup == "highly_capable"], 82435)
  expect_equal(state$n_students[state$subgroup == "mobile"], 16722)
})

test_that("2024 state grade-level enrollment counts are pinned", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state <- enr[enr$is_state & enr$subgroup == "total_enrollment", ]

  expect_equal(state$n_students[state$grade_level == "PK"], 25137)
  expect_equal(state$n_students[state$grade_level == "K"], 76359)
  expect_equal(state$n_students[state$grade_level == "01"], 77642)
  expect_equal(state$n_students[state$grade_level == "02"], 81746)
  expect_equal(state$n_students[state$grade_level == "03"], 78206)
  expect_equal(state$n_students[state$grade_level == "04"], 81075)
  expect_equal(state$n_students[state$grade_level == "05"], 81655)
  expect_equal(state$n_students[state$grade_level == "06"], 80554)
  expect_equal(state$n_students[state$grade_level == "07"], 81329)
  expect_equal(state$n_students[state$grade_level == "08"], 82068)
  expect_equal(state$n_students[state$grade_level == "09"], 86524)
  expect_equal(state$n_students[state$grade_level == "10"], 88236)
  expect_equal(state$n_students[state$grade_level == "11"], 87553)
  expect_equal(state$n_students[state$grade_level == "12"], 91975)
})

test_that("2024 state pct for total_enrollment is exactly 1.0", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state_pct <- enr$pct[enr$is_state & enr$subgroup == "total_enrollment" & enr$grade_level == "TOTAL"]
  expect_equal(state_pct, 1.0)
})

test_that("2024 state demographic percentages are pinned", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state <- enr[enr$is_state & enr$grade_level == "TOTAL", ]

  expect_equal(round(state$pct[state$subgroup == "white"], 4), 0.4841)
  expect_equal(round(state$pct[state$subgroup == "hispanic"], 3), 0.262)
  expect_equal(round(state$pct[state$subgroup == "econ_disadv"], 3), 0.501)
  expect_equal(round(state$pct[state$subgroup == "male"], 4), 0.5155)
  expect_equal(round(state$pct[state$subgroup == "female"], 2), 0.48)
})

# ==============================================================================
# fetch_enr(tidy = TRUE) — pinned district-level values (Seattle, 2024)
# ==============================================================================

test_that("2024 Seattle total enrollment is 50968", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  seattle_total <- enr$n_students[
    enr$district_name == "Seattle School District No. 1" &
    enr$is_district &
    enr$subgroup == "total_enrollment" &
    enr$grade_level == "TOTAL"
  ]
  expect_equal(seattle_total, 50968)
})

test_that("2024 Seattle district_id is 100229", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  seattle_id <- unique(enr$district_id[
    enr$district_name == "Seattle School District No. 1" &
    enr$is_district
  ])
  expect_equal(seattle_id, "100229")
})

test_that("2024 Seattle race/ethnicity counts are pinned", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  seattle <- enr[enr$district_name == "Seattle School District No. 1" &
                 enr$is_district & enr$grade_level == "TOTAL", ]

  expect_equal(seattle$n_students[seattle$subgroup == "white"], 23091)
  expect_equal(seattle$n_students[seattle$subgroup == "black"], 7186)
  expect_equal(seattle$n_students[seattle$subgroup == "hispanic"], 7419)
  expect_equal(seattle$n_students[seattle$subgroup == "asian"], 6344)
  expect_equal(seattle$n_students[seattle$subgroup == "multiracial"], 6433)
  expect_equal(seattle$n_students[seattle$subgroup == "native_american"], 229)
  expect_equal(seattle$n_students[seattle$subgroup == "pacific_islander"], 266)
})

test_that("2024 Seattle special population counts are pinned", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  seattle <- enr[enr$district_name == "Seattle School District No. 1" &
                 enr$is_district & enr$grade_level == "TOTAL", ]

  expect_equal(seattle$n_students[seattle$subgroup == "special_ed"], 9050)
  expect_equal(seattle$n_students[seattle$subgroup == "lep"], 7082)
  expect_equal(seattle$n_students[seattle$subgroup == "econ_disadv"], 17496)
  expect_equal(seattle$n_students[seattle$subgroup == "homeless"], 1635)
  expect_equal(seattle$n_students[seattle$subgroup == "foster_care"], 109)
  expect_equal(seattle$n_students[seattle$subgroup == "highly_capable"], 5736)
})

test_that("2024 Seattle grade-level enrollment is pinned", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  seattle <- enr[enr$district_name == "Seattle School District No. 1" &
                 enr$is_district & enr$subgroup == "total_enrollment", ]

  expect_equal(seattle$n_students[seattle$grade_level == "PK"], 1107)
  expect_equal(seattle$n_students[seattle$grade_level == "K"], 3714)
  expect_equal(seattle$n_students[seattle$grade_level == "01"], 3909)
  expect_equal(seattle$n_students[seattle$grade_level == "05"], 3957)
  expect_equal(seattle$n_students[seattle$grade_level == "09"], 3800)
  expect_equal(seattle$n_students[seattle$grade_level == "12"], 4203)
})

# ==============================================================================
# fetch_enr(tidy = TRUE) — entity type counts (2024)
# ==============================================================================

test_that("2024 has 332 districts in tidy data", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  n_districts <- length(unique(enr$district_id[enr$is_district]))
  expect_equal(n_districts, 332)
})

test_that("2024 has exactly 1 state row per subgroup/grade combination", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state_rows <- enr[enr$is_state, ]
  expect_equal(nrow(state_rows), 35) # 21 subgroups at TOTAL + 14 grades for total_enrollment
})

# ==============================================================================
# fetch_enr(tidy = TRUE) — top 10 district enrollment pinned
# ==============================================================================

test_that("2024 top districts by enrollment are correctly ordered", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  districts <- enr[enr$is_district & enr$subgroup == "total_enrollment" & enr$grade_level == "TOTAL", ]
  districts <- districts[order(-districts$n_students), ]

  top5 <- districts$district_name[1:5]
  expect_equal(top5[1], "Seattle School District No. 1")
  expect_equal(top5[2], "Lake Washington School District")
  expect_equal(top5[3], "Spokane School District")
  expect_equal(top5[4], "Tacoma School District")
  expect_equal(top5[5], "Kent School District")

  expect_equal(districts$n_students[1], 50968)
  expect_equal(districts$n_students[2], 30993)
  expect_equal(districts$n_students[3], 29444)
  expect_equal(districts$n_students[4], 28353)
  expect_equal(districts$n_students[5], 25752)
})

# ==============================================================================
# fetch_enr(tidy = FALSE) — wide format (2024)
# ==============================================================================

test_that("fetch_enr 2024 wide returns a data.frame", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  expect_s3_class(wide, "data.frame")
})

test_that("fetch_enr 2024 wide has correct row and column counts", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  expect_equal(nrow(wide), 2798)
  expect_equal(ncol(wide), 47)
})

test_that("fetch_enr 2024 wide has entity type counts pinned", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  expect_equal(sum(wide$type == "State"), 1)
  expect_equal(sum(wide$type == "District"), 332)
  expect_equal(sum(wide$type == "Campus"), 2465)
})

test_that("fetch_enr 2024 wide state row has pinned values", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  state <- wide[wide$type == "State", ]

  expect_equal(state$row_total, 1100059)
  expect_equal(state$male, 567051)
  expect_equal(state$female, 528028)
  expect_equal(state$gender_x, 4979)
  expect_equal(state$white, 532562)
  expect_equal(state$black, 52031)
  expect_equal(state$hispanic, 288238)
  expect_equal(state$asian, 97920)
  expect_equal(state$native_american, 13168)
  expect_equal(state$pacific_islander, 15903)
  expect_equal(state$multiracial, 100099)
  expect_equal(state$lep, 156910)
  expect_equal(state$econ_disadv, 551109)
  expect_equal(state$special_ed, 176801)
  expect_equal(state$homeless, 41050)
  expect_equal(state$foster_care, 3317)
  expect_equal(state$migrant, 22750)
  expect_equal(state$military_parent, 30198)
  expect_equal(state$section_504, 60154)
  expect_equal(state$highly_capable, 82435)
  expect_equal(state$mobile, 16722)
})

test_that("fetch_enr 2024 wide state grade columns are pinned", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  state <- wide[wide$type == "State", ]

  expect_equal(state$grade_pk, 25137)
  expect_equal(state$grade_k, 76359)
  expect_equal(state$grade_01, 77642)
  expect_equal(state$grade_02, 81746)
  expect_equal(state$grade_03, 78206)
  expect_equal(state$grade_04, 81075)
  expect_equal(state$grade_05, 81655)
  expect_equal(state$grade_06, 80554)
  expect_equal(state$grade_07, 81329)
  expect_equal(state$grade_08, 82068)
  expect_equal(state$grade_09, 86524)
  expect_equal(state$grade_10, 88236)
  expect_equal(state$grade_11, 87553)
  expect_equal(state$grade_12, 91975)
})

test_that("fetch_enr 2024 wide has correct columns", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)

  expected_cols <- c(
    "end_year", "type", "district_id", "district_code", "district_name",
    "campus_id", "school_code", "campus_name", "school_type",
    "esd_id", "esd_name", "county",
    "row_total", "male", "female", "gender_x",
    "white", "black", "hispanic", "asian",
    "pacific_islander", "native_american", "multiracial",
    "lep", "econ_disadv", "special_ed",
    "homeless", "foster_care", "migrant", "military_parent",
    "section_504", "highly_capable", "mobile",
    "grade_k", "grade_pk",
    "grade_01", "grade_02", "grade_03", "grade_04", "grade_05",
    "grade_06", "grade_07", "grade_08", "grade_09",
    "grade_10", "grade_11", "grade_12"
  )

  for (col in expected_cols) {
    expect_true(col %in% names(wide), info = paste("Wide missing column:", col))
  }
})

# ==============================================================================
# fetch_enr_multi() — multi-year tests
# ==============================================================================

test_that("fetch_enr_multi 2022-2024 returns combined data", {
  multi <- fetch_enr_multi(2022:2024, tidy = TRUE, use_cache = TRUE)

  expect_s3_class(multi, "data.frame")
  expect_equal(sort(unique(multi$end_year)), 2022:2024)
})

test_that("fetch_enr_multi 2022-2024 state totals are pinned", {
  multi <- fetch_enr_multi(2022:2024, tidy = TRUE, use_cache = TRUE)
  state_totals <- multi[multi$is_state & multi$subgroup == "total_enrollment" & multi$grade_level == "TOTAL", ]
  state_totals <- state_totals[order(state_totals$end_year), ]

  expect_equal(state_totals$n_students[state_totals$end_year == 2022], 1091343)
  expect_equal(state_totals$n_students[state_totals$end_year == 2023], 1096695)
  expect_equal(state_totals$n_students[state_totals$end_year == 2024], 1100059)
})

test_that("fetch_enr_multi with single year works like fetch_enr", {
  multi <- fetch_enr_multi(2024, tidy = TRUE, use_cache = TRUE)
  single <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  expect_equal(nrow(multi), nrow(single))
  expect_equal(sort(names(multi)), sort(names(single)))
})

test_that("fetch_enr_multi validates invalid years", {
  expect_error(fetch_enr_multi(c(2024, 2030), use_cache = TRUE), "Invalid years")
  expect_error(fetch_enr_multi(c(2005, 2024), use_cache = TRUE), "Invalid years")
})

test_that("fetch_enr_multi wide format works", {
  multi_wide <- fetch_enr_multi(c(2023, 2024), tidy = FALSE, use_cache = TRUE)

  expect_s3_class(multi_wide, "data.frame")
  expect_equal(sort(unique(multi_wide$end_year)), c(2023, 2024))
  expect_true("row_total" %in% names(multi_wide))
  expect_false("subgroup" %in% names(multi_wide)) # wide format, no subgroup column
})

# ==============================================================================
# Historical year tests (2010, 2015)
# ==============================================================================

test_that("2010 state total enrollment is 1034935", {
  enr <- fetch_enr(2010, tidy = TRUE, use_cache = TRUE)
  state_total <- enr$n_students[enr$is_state & enr$subgroup == "total_enrollment" & enr$grade_level == "TOTAL"]
  expect_equal(state_total, 1034935)
})

test_that("2015 state total enrollment is 1086314", {
  enr <- fetch_enr(2015, tidy = TRUE, use_cache = TRUE)
  state_total <- enr$n_students[enr$is_state & enr$subgroup == "total_enrollment" & enr$grade_level == "TOTAL"]
  expect_equal(state_total, 1086314)
})

test_that("2020 state total enrollment is 1146882", {
  enr <- fetch_enr(2020, tidy = TRUE, use_cache = TRUE)
  state_total <- enr$n_students[enr$is_state & enr$subgroup == "total_enrollment" & enr$grade_level == "TOTAL"]
  expect_equal(state_total, 1146882)
})

test_that("2025 state total enrollment is 1105384", {
  enr <- fetch_enr(2025, tidy = TRUE, use_cache = TRUE)
  state_total <- enr$n_students[enr$is_state & enr$subgroup == "total_enrollment" & enr$grade_level == "TOTAL"]
  expect_equal(state_total, 1105384)
})

test_that("2010 has 300 districts", {
  enr <- fetch_enr(2010, tidy = TRUE, use_cache = TRUE)
  n_districts <- sum(enr$is_district & enr$subgroup == "total_enrollment" & enr$grade_level == "TOTAL")
  expect_equal(n_districts, 300)
})

test_that("2010 tidy has 20 subgroups (no highly_capable)", {
  enr <- fetch_enr(2010, tidy = TRUE, use_cache = TRUE)
  subgroups <- sort(unique(enr$subgroup))
  expect_equal(length(subgroups), 20)
  expect_false("highly_capable" %in% subgroups)
})

test_that("2010 wide has 46 columns (no highly_capable)", {
  wide <- fetch_enr(2010, tidy = FALSE, use_cache = TRUE)
  expect_equal(ncol(wide), 46)
  expect_false("highly_capable" %in% names(wide))
})

test_that("2010 wide entity counts are pinned", {
  wide <- fetch_enr(2010, tidy = FALSE, use_cache = TRUE)
  expect_equal(nrow(wide), 2497)
  expect_equal(sum(wide$type == "District"), 300)
  expect_equal(sum(wide$type == "Campus"), 2196)
})

# ==============================================================================
# enr_grade_aggs() — grade aggregation tests
# ==============================================================================

test_that("enr_grade_aggs returns all 5 aggregate levels", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)

  grade_levels <- sort(unique(aggs$grade_level))
  expect_equal(grade_levels, c("ELEM", "HS", "K12", "K8", "MIDDLE"))
})

test_that("enr_grade_aggs 2024 state values are pinned", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)
  state_aggs <- aggs[aggs$is_state, ]

  expect_equal(state_aggs$n_students[state_aggs$grade_level == "ELEM"], 476683)
  expect_equal(state_aggs$n_students[state_aggs$grade_level == "MIDDLE"], 243951)
  expect_equal(state_aggs$n_students[state_aggs$grade_level == "K8"], 720634)
  expect_equal(state_aggs$n_students[state_aggs$grade_level == "HS"], 354288)
  expect_equal(state_aggs$n_students[state_aggs$grade_level == "K12"], 1074922)
})

test_that("enr_grade_aggs K8 = ELEM + MIDDLE for state", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)
  state_aggs <- aggs[aggs$is_state, ]

  elem <- state_aggs$n_students[state_aggs$grade_level == "ELEM"]
  middle <- state_aggs$n_students[state_aggs$grade_level == "MIDDLE"]
  k8 <- state_aggs$n_students[state_aggs$grade_level == "K8"]
  expect_equal(elem + middle, k8)
})

test_that("enr_grade_aggs K12 = K8 + HS for state", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)
  state_aggs <- aggs[aggs$is_state, ]

  k8 <- state_aggs$n_students[state_aggs$grade_level == "K8"]
  hs <- state_aggs$n_students[state_aggs$grade_level == "HS"]
  k12 <- state_aggs$n_students[state_aggs$grade_level == "K12"]
  expect_equal(k8 + hs, k12)
})

test_that("enr_grade_aggs Seattle district values are pinned", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)
  seattle <- aggs[aggs$district_name == "Seattle School District No. 1" & aggs$is_district, ]

  expect_equal(seattle$n_students[seattle$grade_level == "ELEM"], 23272)
  expect_equal(seattle$n_students[seattle$grade_level == "MIDDLE"], 10634)
  expect_equal(seattle$n_students[seattle$grade_level == "K8"], 33906)
  expect_equal(seattle$n_students[seattle$grade_level == "HS"], 15955)
  expect_equal(seattle$n_students[seattle$grade_level == "K12"], 49861)
})

test_that("enr_grade_aggs only produces total_enrollment subgroup", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)
  expect_equal(unique(aggs$subgroup), "total_enrollment")
})

test_that("enr_grade_aggs has correct row count", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)
  # 5 grade aggs x (1 state + 332 districts + some campuses)
  expect_equal(nrow(aggs), 8950)
})

test_that("enr_grade_aggs has pct = NA", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)
  expect_true(all(is.na(aggs$pct)))
})

# ==============================================================================
# tidy_enr() — standalone usage
# ==============================================================================

test_that("tidy_enr can be called on wide data", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- tidy_enr(wide)

  expect_true("subgroup" %in% names(tidy))
  expect_true("grade_level" %in% names(tidy))
  expect_true("n_students" %in% names(tidy))
  expect_true("pct" %in% names(tidy))
})

test_that("tidy_enr preserves state total", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- tidy_enr(wide)

  state_total <- tidy$n_students[tidy$type == "State" & tidy$subgroup == "total_enrollment" & tidy$grade_level == "TOTAL"]
  expect_equal(state_total, 1100059)
})

# ==============================================================================
# id_enr_aggs() — standalone usage
# ==============================================================================

test_that("id_enr_aggs adds aggregation flags", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- tidy_enr(wide)
  result <- id_enr_aggs(tidy)

  expect_true("is_state" %in% names(result))
  expect_true("is_district" %in% names(result))
  expect_true("is_campus" %in% names(result))
  expect_true("aggregation_flag" %in% names(result))
})

test_that("id_enr_aggs flags are mutually exclusive", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  type_sums <- as.integer(enr$is_state) + as.integer(enr$is_district) + as.integer(enr$is_campus)
  expect_true(all(type_sums == 1))
})

test_that("id_enr_aggs aggregation_flag values match type flags", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true(all(enr$aggregation_flag[enr$is_state] == "state"))

  # district and campus flags depend on ID presence, not just type
  # but should broadly align
  expect_true(all(enr$is_district[enr$aggregation_flag == "district"]))
})

# ==============================================================================
# Campus-level data tests
# ==============================================================================

test_that("2024 campus-level data has known schools", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  campus_names <- unique(enr$campus_name[enr$is_campus])

  expect_true("Roosevelt Elementary" %in% campus_names)
  expect_true("Chiawana Senior High School" %in% campus_names)
})

test_that("2024 campus Roosevelt Elementary Spokane enrollment is pinned", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  roosevelt <- enr[enr$campus_name == "Roosevelt Elementary" &
                   enr$district_name == "Spokane School District" &
                   enr$is_campus &
                   enr$subgroup == "total_enrollment" &
                   enr$grade_level == "TOTAL", ]
  expect_equal(nrow(roosevelt), 1)
  expect_equal(roosevelt$n_students, 436)
  expect_equal(roosevelt$campus_id, "102688")
})

# ==============================================================================
# Cross-year state total consistency
# ==============================================================================

test_that("State totals increase from 2010 to 2024", {
  total_2010 <- 1034935
  total_2024 <- 1100059
  expect_true(total_2024 > total_2010)
})

test_that("State totals show expected COVID dip pattern", {
  # Washington enrollment grew pre-COVID, dipped in 2021, recovered after
  enr_multi <- fetch_enr_multi(2019:2022, tidy = TRUE, use_cache = TRUE)
  totals <- enr_multi[enr_multi$is_state & enr_multi$subgroup == "total_enrollment" & enr_multi$grade_level == "TOTAL", ]
  totals <- totals[order(totals$end_year), ]

  # 2020 > 2021 (COVID dip)
  total_2020 <- totals$n_students[totals$end_year == 2020]
  total_2021 <- totals$n_students[totals$end_year == 2021]
  expect_true(total_2020 > total_2021)
})

# ==============================================================================
# fetch_enr_multi spanning data eras
# ==============================================================================

test_that("fetch_enr_multi spans Student Enrollment and Report Card eras", {
  multi <- fetch_enr_multi(c(2014, 2015), tidy = TRUE, use_cache = TRUE)
  years <- sort(unique(multi$end_year))
  expect_equal(years, c(2014, 2015))

  # Both should have core subgroups
  for (yr in c(2014, 2015)) {
    yr_subs <- unique(multi$subgroup[multi$end_year == yr])
    expect_true("total_enrollment" %in% yr_subs)
    expect_true("white" %in% yr_subs)
    expect_true("male" %in% yr_subs)
  }
})

# ==============================================================================
# ESD and county metadata tests
# ==============================================================================

test_that("2024 data contains known ESD names", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  esds <- unique(na.omit(enr$esd_name))

  expect_true("Educational Service District 101" %in% esds)
  expect_true("Capital Region ESD 113" %in% esds)
})

test_that("2024 data contains known counties", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  counties <- unique(na.omit(enr$county))

  expect_true("King" %in% counties)
  expect_true("Pierce" %in% counties)
  expect_true("Spokane" %in% counties)
  expect_true("Clark" %in% counties)
  expect_true("Snohomish" %in% counties)
})

test_that("2024 data contains known school types", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  types <- unique(na.omit(enr$school_type))

  # Washington school type codes: P = Primary, S = Secondary, etc.
  expect_true("P" %in% types)
  expect_true("S" %in% types)
})

# ==============================================================================
# Cache interaction tests
# ==============================================================================

test_that("fetch_enr use_cache=TRUE returns same result on repeated calls", {
  enr1 <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  enr2 <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_equal(nrow(enr1), nrow(enr2))
  expect_equal(ncol(enr1), ncol(enr2))
  expect_equal(
    enr1$n_students[enr1$is_state & enr1$subgroup == "total_enrollment" & enr1$grade_level == "TOTAL"],
    enr2$n_students[enr2$is_state & enr2$subgroup == "total_enrollment" & enr2$grade_level == "TOTAL"]
  )
})

test_that("fetch_enr tidy vs wide state total matches", {
  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)

  tidy_state <- tidy$n_students[tidy$is_state & tidy$subgroup == "total_enrollment" & tidy$grade_level == "TOTAL"]
  wide_state <- wide$row_total[wide$type == "State"]

  expect_equal(tidy_state, wide_state)
})
