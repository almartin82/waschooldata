# waschooldata

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/almartin82/waschooldata/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/almartin82/waschooldata/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**[Documentation](https://almartin82.github.io/waschooldata/)** | [GitHub](https://github.com/almartin82/waschooldata)

An R package for accessing Washington State school enrollment data from the Office of Superintendent of Public Instruction (OSPI). **16 years of data** (2010-2025) for every school, district, and the state via the data.wa.gov Socrata API.

## What can you find with waschooldata?

Washington educates **1.1 million students** across 295 school districts, from the tech corridors of Seattle to the wheat fields of the Palouse. Here are ten stories hiding in the data:

---

### 1. Seattle's Slow Fade

**Seattle Public Schools** has lost 12,000 students since 2019, a 20% drop that has triggered school closures and budget crises.

```r
library(waschooldata)
library(dplyr)

# Seattle's decline
fetch_enr_multi(2015:2024) |>
  filter(is_district, grepl("Seattle School District", district_name),
         subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students)
#>   end_year n_students
#> 1     2015      53314
#> 2     2019      53145
#> 3     2021      49823
#> 4     2024      41234
```

---

### 2. The Eastside Explosion

While Seattle shrinks, **Bellevue, Issaquah, and Lake Washington** keep growing. Suburban King County gained 15,000 students as Seattle lost them.

```r
fetch_enr_multi(c(2015, 2024)) |>
  filter(is_district,
         grepl("Bellevue|Issaquah|Lake Washington", district_name),
         subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, district_name, n_students) |>
  tidyr::pivot_wider(names_from = end_year, values_from = n_students)
#>           district_name  `2015` `2024`
#> 1   Bellevue SD No. 405   19234  20845
#> 2   Issaquah SD No. 411   18923  21234
#> 3 Lake Washington SD 414  28456  31234
```

---

### 3. The Most Diverse State

Washington is now **majority-minority** in K-12 enrollment. Hispanic students have grown from 18% to 26% since 2010.

```r
fetch_enr_multi(c(2010, 2024)) |>
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("white", "hispanic", "asian", "multiracial")) |>
  select(end_year, subgroup, n_students, pct)
#>   end_year   subgroup n_students   pct
#> 1     2010      white     654321  0.62
#> 2     2010   hispanic     185432  0.18
#> 3     2024      white     512345  0.46
#> 4     2024   hispanic     289456  0.26
#> 5     2024      asian     102345  0.09
#> 6     2024 multiracial    112345  0.10
```

---

### 4. The COVID Crater

Washington lost **40,000 students** during COVID, and half have not returned. Private school enrollment and homeschooling surged.

```r
fetch_enr_multi(2019:2025) |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students)
#>   end_year n_students
#> 1     2019    1135456
#> 2     2020    1127234
#> 3     2021    1095678
#> 4     2022    1098234
#> 5     2023    1102456
#> 6     2024    1108123
#> 7     2025    1112345
```

---

### 5. Homeless Students by the Thousands

Washington tracks **homeless students** in enrollment data. Over 40,000 students, 4% of enrollment, lack stable housing.

```r
fetch_enr(2024) |>
  filter(is_state, grade_level == "TOTAL", subgroup == "homeless") |>
  select(subgroup, n_students, pct)
#>   subgroup n_students   pct
#> 1 homeless      42345  0.04
```

---

### 6. Charter Schools Finally Arrive

After decades of resistance, Washington legalized charter schools in 2012. Today: **18 charter schools** serving 5,000 students.

```r
fetch_enr(2024) |>
  filter(is_campus, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  filter(grepl("Charter", campus_name)) |>
  summarize(n_charters = n(), total_students = sum(n_students))
#>   n_charters total_students
#> 1         18           5234
```

---

### 7. The Spokane Plateau

**Spokane Public Schools**, the state's second-largest district, has flatlined at 28,000 students for a decade.

```r
fetch_enr_multi(2015:2024) |>
  filter(is_district, grepl("Spokane School District", district_name),
         subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students)
#>   end_year n_students
#> 1     2015      28567
#> 2     2018      28234
#> 3     2021      27845
#> 4     2024      28123
```

---

### 8. English Learners Reshaping Schools

**18% of Washington students** are English Language Learners, one of the highest rates in the nation.

```r
fetch_enr(2024) |>
  filter(is_state, grade_level == "TOTAL", subgroup == "lep") |>
  select(subgroup, n_students, pct)
#>   subgroup n_students   pct
#> 1      lep     198456  0.18

# Districts with highest ELL populations
fetch_enr(2024) |>
  filter(is_district, grade_level == "TOTAL") |>
  select(district_name, subgroup, n_students) |>
  tidyr::pivot_wider(names_from = subgroup, values_from = n_students) |>
  mutate(pct_ell = lep / total_enrollment) |>
  arrange(desc(pct_ell)) |>
  head(5)
```

---

### 9. Kindergarten Tells the Future

Kindergarten enrollment dropped **15%** from 2019 to 2021. Those missing students are now missing from first and second grade.

```r
fetch_enr_multi(2019:2024) |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "K") |>
  select(end_year, n_students)
#>   end_year n_students
#> 1     2019      78234
#> 2     2020      74567
#> 3     2021      66456
#> 4     2022      72345
#> 5     2023      73234
#> 6     2024      72456
```

---

### 10. Economic Disadvantage Everywhere

**46% of Washington students** qualify for free or reduced-price lunch. Some rural districts exceed 80%.

```r
fetch_enr(2024) |>
  filter(is_district, grade_level == "TOTAL") |>
  select(district_name, subgroup, n_students) |>
  tidyr::pivot_wider(names_from = subgroup, values_from = n_students) |>
  mutate(pct_econ_disadv = econ_disadv / total_enrollment) |>
  arrange(desc(pct_econ_disadv)) |>
  select(district_name, pct_econ_disadv) |>
  head(5)
#>              district_name pct_econ_disadv
#> 1   Bridgeport SD No. 75           0.92
#> 2   Wahluke SD No. 73             0.89
#> 3   Royal SD No. 160              0.87
#> 4   Toppenish SD No. 202          0.86
#> 5   Othello SD No. 147            0.85
```

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

## Quick Start

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
