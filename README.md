# waschooldata

<!-- badges: start -->
[![R-CMD-check](https://github.com/almartin82/waschooldata/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/almartin82/waschooldata/actions/workflows/R-CMD-check.yaml)
[![Python Tests](https://github.com/almartin82/waschooldata/actions/workflows/python-test.yaml/badge.svg)](https://github.com/almartin82/waschooldata/actions/workflows/python-test.yaml)
[![pkgdown](https://github.com/almartin82/waschooldata/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/almartin82/waschooldata/actions/workflows/pkgdown.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**[Documentation](https://almartin82.github.io/waschooldata/)** | [GitHub](https://github.com/almartin82/waschooldata)

Fetch and analyze Washington school enrollment data from [OSPI](https://data.wa.gov/) in R or Python. **16 years of data** (2010-2025) for every school, district, and the state via the data.wa.gov Socrata API.

## What can you find with waschooldata?

Washington educates **1.1 million students** across 295 school districts, from the tech corridors of Seattle to the wheat fields of the Palouse. The [enrollment hooks vignette](https://almartin82.github.io/waschooldata/articles/enrollment_hooks.html) explores key trends across 16 years of data (2010-2025).

---

## Enrollment Visualizations

<img src="https://almartin82.github.io/waschooldata/articles/enrollment_hooks_files/figure-html/statewide-chart-1.png" alt="Washington statewide enrollment trends" width="600">

<img src="https://almartin82.github.io/waschooldata/articles/enrollment_hooks_files/figure-html/top-districts-chart-1.png" alt="Top Washington districts" width="600">

See the [full vignette](https://almartin82.github.io/waschooldata/articles/enrollment_hooks.html) for more insights.

## Installation

```r
# install.packages("devtools")
devtools::install_github("almartin82/waschooldata")
```

## R Quick Start

```r
library(waschooldata)
library(dplyr)

# Get 2024 enrollment data (2023-24 school year)
enr <- fetch_enr(2024)

# Statewide total
enr |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  pull(n_students)
#> 1,108,123

# Top 5 districts
enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  arrange(desc(n_students)) |>
  select(district_name, n_students) |>
  head(5)
```

## Python Quick Start

```python
import pywaschooldata as wa

# Fetch 2024 data (2023-24 school year)
enr = wa.fetch_enr(2024)

# Statewide total
total = enr[(enr['is_state'] == True) &
            (enr['subgroup'] == 'total_enrollment') &
            (enr['grade_level'] == 'TOTAL')]['n_students'].sum()
print(f"{total:,} students")
#> 1,108,123 students

# Get multiple years
enr_multi = wa.fetch_enr_multi([2020, 2021, 2022, 2023, 2024])

# Check available years
years = wa.get_available_years()
print(f"Data available: {years['min_year']}-{years['max_year']}")
#> Data available: 2010-2025
```

## Data Format

`fetch_enr()` returns tidy (long) format by default:

| Column | Description |
|--------|-------------|
| `end_year` | School year end (e.g., 2024 for 2023-24) |
| `district_id` | District organization ID |
| `campus_id` | School organization ID |
| `type` | "State", "District", or "Campus" |
| `district_name`, `campus_name` | Names |
| `county` | County name |
| `grade_level` | "TOTAL", "PK", "K", "01"..."12" |
| `subgroup` | Demographic/population group |
| `n_students` | Enrollment count |
| `pct` | Percentage of total |

### Subgroups Available

**Demographics**: `white`, `black`, `hispanic`, `asian`, `pacific_islander`, `native_american`, `multiracial`

**Populations**: `econ_disadv`, `lep`, `special_ed`, `homeless`, `foster_care`, `migrant`, `military`, `highly_capable`

## Data Availability

| Era | Years | Source |
|-----|-------|--------|
| Report Card | 2015-2025 | data.wa.gov (full detail) |
| Student Enrollment | 2010-2014 | data.wa.gov (fewer ID columns) |

**16 years total** across ~2,400 schools and 295 districts.

## Part of the State Schooldata Project

A simple, consistent interface for accessing state-published school data in Python and R.

**All 50 state packages:** [github.com/almartin82](https://github.com/almartin82?tab=repositories&q=schooldata)

## Author

Andy Martin (almartin@gmail.com)
[github.com/almartin82](https://github.com/almartin82)

## License

MIT
