# waschooldata

<!-- badges: start -->
[![R-CMD-check](https://github.com/almartin82/waschooldata/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/almartin82/waschooldata/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

An R package for fetching, processing, and analyzing school enrollment data from Washington State's Office of Superintendent of Public Instruction (OSPI).

## Installation

You can install the development version of waschooldata from GitHub:

```r
# install.packages("devtools")
devtools::install_github("almartin82/waschooldata")
```

## Quick Start

```r
library(waschooldata)

# Fetch 2024 enrollment data (2023-24 school year)
enr_2024 <- fetch_enr(2024)

# Get multiple years
enr_multi <- fetch_enr_multi(2020:2024)

# Get wide format (one row per organization)
enr_wide <- fetch_enr(2024, tidy = FALSE)

# See available years
get_available_years()
```

## Data Source

Data is sourced from the **Washington State Report Card** via the [data.wa.gov](https://data.wa.gov) open data portal (Socrata API).

- **Primary Data Portal**: https://data.wa.gov
- **Report Card Website**: https://reportcard.ospi.k12.wa.us/
- **OSPI Data Portal**: https://ospi.k12.wa.us/data-reporting/data-portal

## Data Availability

### Years Available

| Format Era | Years | Source | Notes |
|------------|-------|--------|-------|
| Socrata API | 2019-2025 | data.wa.gov | Current implementation |

**Total**: 7 years of data (2018-19 through 2024-25 school years)

### What's Included

- **Aggregation Levels**: State, District, School
- **Demographics**: Gender, Race/Ethnicity (7 categories)
- **Special Populations**:
  - English Language Learners (ELL)
  - Low Income (Economically Disadvantaged)
  - Students with Disabilities (Special Education)
  - Homeless
  - Foster Care
  - Migrant
  - Military Connected
  - Section 504
  - Highly Capable
  - Mobile Students
- **Grade Levels**: Pre-K through 12th grade

### Enrollment Count Date

The first business day in October is used as the enrollment count date for all schools and districts in Washington state. Data reflects CEDARS (Comprehensive Education Data and Research System) enrollment as of that date.

### What's NOT Available (in this package)

- Data before 2018-19 school year (available via SAFS Excel files on OSPI website)
- Student-level data (only aggregate counts)
- Financial data (separate OSPI data system)
- Assessment scores (separate Report Card datasets)

### Known Caveats

- **Small cell suppression**: Counts of fewer than 10 students may be suppressed to protect student privacy
- **Gender X**: Non-binary gender reporting began in recent years; earlier years show only Male/Female
- **Race/Ethnicity categories**: Federal reporting categories; "Two or More Races" available throughout
- **Charter schools**: Included in district and school counts
- **Private schools**: Not included (public schools only)

## Data Structure

### Wide Format (`tidy = FALSE`)

Returns one row per organization (state, district, or school) with columns:

| Column | Type | Description |
|--------|------|-------------|
| end_year | integer | School year end (2024 = 2023-24) |
| type | character | "State", "District", or "Campus" |
| district_id | character | District organization ID |
| district_name | character | District name |
| campus_id | character | School organization ID |
| campus_name | character | School name |
| county | character | County name |
| row_total | integer | Total enrollment |
| white, black, hispanic, asian, pacific_islander, native_american, multiracial | integer | Race/ethnicity counts |
| male, female, gender_x | integer | Gender counts |
| lep, econ_disadv, special_ed, homeless, foster_care, migrant | integer | Special population counts |
| grade_pk, grade_k, grade_01 through grade_12 | integer | Grade-level enrollment |

### Tidy Format (`tidy = TRUE`, default)

Returns long format with one row per organization/subgroup/grade combination:

| Column | Type | Description |
|--------|------|-------------|
| end_year | integer | School year end |
| type | character | Aggregation level |
| district_id, campus_id | character | Identifiers |
| district_name, campus_name | character | Names |
| grade_level | character | "TOTAL", "PK", "K", "01"-"12" |
| subgroup | character | "total_enrollment", "white", "hispanic", etc. |
| n_students | integer | Student count |
| pct | numeric | Percentage of total (0-1 scale) |
| is_state, is_district, is_campus | logical | Aggregation level flags |

## Washington Identifiers

- **District Organization ID**: Unique 5-digit identifier for each district
- **School Organization ID**: Unique 5-digit identifier for each school
- **District Code**: Short alphanumeric code
- **School Code**: Short alphanumeric code
- **ESD (Educational Service District)**: Regional service agency identifier

## Examples

### State Total Enrollment Trend

```r
library(waschooldata)
library(dplyr)
library(ggplot2)

# Get 5 years of data
enr <- fetch_enr_multi(2020:2024)

# State enrollment by year
state_trend <- enr %>%
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  select(end_year, n_students)

ggplot(state_trend, aes(x = end_year, y = n_students)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Washington State K-12 Enrollment",
    x = "School Year (End)",
    y = "Total Students"
  )
```

### District Demographics

```r
# Get Seattle School District demographics
seattle <- fetch_enr(2024) %>%
  filter(
    district_name == "Seattle School District No. 1",
    is_district,
    grade_level == "TOTAL",
    subgroup %in% c("white", "black", "hispanic", "asian", "multiracial")
  )

print(seattle %>% select(subgroup, n_students, pct))
```

### Grade-Level Analysis

```r
# Elementary vs High School enrollment
enr_2024 <- fetch_enr(2024)

grade_aggs <- enr_grade_aggs(enr_2024) %>%
  filter(is_state) %>%
  select(grade_level, n_students)

print(grade_aggs)
```

## Caching

Downloaded data is cached locally to avoid repeated downloads:

```r
# Check cache status
cache_status()

# Clear cache for specific year
clear_cache(2024)

# Clear all cached data
clear_cache()

# Force fresh download (bypass cache)
enr <- fetch_enr(2024, use_cache = FALSE)
```

Cache files are stored in `rappdirs::user_cache_dir("waschooldata")`.

## Related Packages

- [caschooldata](https://github.com/almartin82/caschooldata) - California school data
- [ilschooldata](https://github.com/almartin82/ilschooldata) - Illinois school data
- [nyschooldata](https://github.com/almartin82/nyschooldata) - New York school data
- [ohschooldata](https://github.com/almartin82/ohschooldata) - Ohio school data
- [paschooldata](https://github.com/almartin82/paschooldata) - Pennsylvania school data
- [txschooldata](https://github.com/almartin82/txschooldata) - Texas school data

## License
MIT
