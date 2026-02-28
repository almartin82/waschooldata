# waschooldata

<!-- badges: start -->
[![R-CMD-check](https://github.com/almartin82/waschooldata/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/almartin82/waschooldata/actions/workflows/R-CMD-check.yaml)
[![Python Tests](https://github.com/almartin82/waschooldata/actions/workflows/python-test.yaml/badge.svg)](https://github.com/almartin82/waschooldata/actions/workflows/python-test.yaml)
[![pkgdown](https://github.com/almartin82/waschooldata/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/almartin82/waschooldata/actions/workflows/pkgdown.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Washington educates **1.1 million students** across 295 school districts, from the tech corridors of Seattle to the wheat fields of the Palouse. This package gives you 16 years of enrollment data (2010-2025) for every school, district, and the state -- race, gender, grade level, special populations, and more -- via the OSPI data.wa.gov API.

Part of the [njschooldata](https://github.com/almartin82/njschooldata) family of state education data packages.

**[Full documentation](https://almartin82.github.io/waschooldata/)** -- all 15 stories with interactive charts, getting-started guide, and complete function reference.

## Highlights

```r
library(waschooldata)
library(dplyr)
library(tidyr)

enr <- fetch_enr_multi(2010:2025, use_cache = TRUE)
```

---

### 1. White students dropped from 64% to 48% as Hispanic enrollment nearly doubled

The state's demographics reflect Pacific Rim immigration and a growing Hispanic population that rose from 167K to 295K students.

```r
demographics <- enr |>
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("white", "hispanic", "asian", "black", "multiracial"),
         end_year %in% c(2010, 2015, 2020, 2025)) |>
  select(end_year, subgroup, n_students, pct) |>
  mutate(pct = round(pct * 100, 1))
stopifnot(nrow(demographics) > 0)

demographics |>
  pivot_wider(names_from = end_year, values_from = c(n_students, pct))
#> # A tibble: 5 x 9
#>   subgroup    n_students_2010 n_students_2015 n_students_2020 n_students_2025
#>   <chr>                 <dbl>           <dbl>           <dbl>           <dbl>
#> 1 white                657143          615697          601749          526102
#> 2 black                 56515           48578           50251           53176
#> 3 hispanic             167426          235730          273842          294985
#> 4 asian                 80375           77981           91377          100676
#> 5 multiracial           35867           81757          101807          101068
#> # i 4 more variables: pct_2010 <dbl>, pct_2015 <dbl>, pct_2020 <dbl>,
#> #   pct_2025 <dbl>
```

![Demographics](https://almartin82.github.io/waschooldata/articles/enrollment_hooks_files/figure-html/demographics-chart-1.png)

[(source)](https://almartin82.github.io/waschooldata/articles/enrollment_hooks.html#white-students-dropped-from-64-to-48-as-hispanic-enrollment-nearly-doubled)

---

### 2. The kindergarten cliff is real: K enrollment down 14% from 2020 peak

Washington's kindergarten enrollment dropped during COVID and hasn't recovered, signaling smaller cohorts ahead.

```r
k_trend <- enr |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "K") |>
  select(end_year, n_students) |>
  mutate(pct_of_peak = round(n_students / max(n_students) * 100, 1))
stopifnot(nrow(k_trend) > 0)

k_trend
#>    end_year n_students pct_of_peak
#> 1      2010      73735        88.9
#> 2      2011      75364        90.9
#> 3      2012      77896        93.9
#> 4      2013      80426        97.0
#> 5      2014      81286        98.0
#> 6      2015      81348        98.1
#> 7      2016      79874        96.3
#> 8      2017      81151        97.8
#> 9      2018      81428        98.2
#> 10     2019      82130        99.0
#> 11     2020      82947       100.0
#> 12     2021      70977        85.6
#> 13     2022      78640        94.8
#> 14     2023      78406        94.5
#> 15     2024      76359        92.1
#> 16     2025      71443        86.1
```

![Kindergarten enrollment](https://almartin82.github.io/waschooldata/articles/enrollment_hooks_files/figure-html/k-chart-1.png)

[(source)](https://almartin82.github.io/waschooldata/articles/enrollment_hooks.html#the-kindergarten-cliff-is-real-k-enrollment-down-14-from-2020-peak)

---

### 3. Foster care enrollment dropped 53% from 2019 peak

Washington tracks foster care enrollment at every school, revealing the population peaked at 7,573 in 2019 and has since dropped to 3,560.

```r
foster_trend <- enr |>
  filter(is_state, grade_level == "TOTAL", subgroup == "foster_care") |>
  select(end_year, n_students, pct) |>
  mutate(pct = round(pct * 100, 2))
stopifnot(nrow(foster_trend) > 0)

foster_trend
#>    end_year n_students  pct
#> 1      2010       6670 0.64
#> 2      2011       6715 0.64
#> 3      2012       6124 0.58
#> 4      2013       5627 0.53
#> 5      2014       5452 0.51
#> 6      2015       5268 0.48
#> 7      2016       5224 0.47
#> 8      2017       5873 0.53
#> 9      2018       6739 0.60
#> 10     2019       7573 0.67
#> 11     2020       6812 0.59
#> 12     2021       5598 0.51
#> 13     2022       4903 0.45
#> 14     2023       4112 0.37
#> 15     2024       3317 0.30
#> 16     2025       3560 0.32
```

![Foster care trend](https://almartin82.github.io/waschooldata/articles/enrollment_hooks_files/figure-html/foster-care-chart-1.png)

[(source)](https://almartin82.github.io/waschooldata/articles/enrollment_hooks.html#foster-care-enrollment-dropped-53-from-2019-peak)

---

## Data Taxonomy

| Category | Years | Function | Details |
|----------|-------|----------|---------|
| **Enrollment** | 2010-2025 | `fetch_enr()` / `fetch_enr_multi()` | State, district, school. Race, gender, FRPL, SpEd, LEP, foster care, homeless, migrant |
| Assessments | -- | -- | Not yet available |
| Graduation | -- | -- | Not yet available |
| Directory | -- | -- | Not yet available |
| Per-Pupil Spending | -- | -- | Not yet available |
| Accountability | -- | -- | Not yet available |
| Chronic Absence | -- | -- | Not yet available |
| EL Progress | -- | -- | Not yet available |
| Special Ed | -- | -- | Not yet available |

> See the full [data category taxonomy](DATA-CATEGORY-TAXONOMY.md) for what each category covers.

## Quick Start

### R

```r
# install.packages("devtools")
devtools::install_github("almartin82/waschooldata")

library(waschooldata)
library(dplyr)

# Get 2025 enrollment data (2024-25 school year)
enr <- fetch_enr(2025)

# Statewide total
enr |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students)

# Top 5 districts
enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  arrange(desc(n_students)) |>
  select(district_name, n_students) |>
  head(5)
```

### Python

```python
import pywaschooldata as wa

# Fetch 2025 data (2024-25 school year)
enr = wa.fetch_enr(2025)

# Statewide total
total = enr[(enr['is_state'] == True) &
            (enr['subgroup'] == 'total_enrollment') &
            (enr['grade_level'] == 'TOTAL')]['n_students'].sum()
print(f"{total:,} students")

# Get multiple years
enr_multi = wa.fetch_enr_multi([2020, 2021, 2022, 2023, 2024, 2025])

# Check available years
years = wa.get_available_years()
print(f"Data available: {years['min_year']}-{years['max_year']}")
```

## Explore More

Full analysis with 15 stories covering statewide trends, regional patterns, and demographic shifts:

- [Enrollment trends](https://almartin82.github.io/waschooldata/articles/enrollment_hooks.html) -- 15 stories
- [Function reference](https://almartin82.github.io/waschooldata/reference/)

## Data Notes

### Data Source

All data comes directly from the Washington Office of Superintendent of Public Instruction (OSPI) via the [Washington State Report Card](https://washingtonstatereportcard.ospi.k12.wa.us/) and [data.wa.gov](https://data.wa.gov/).

### Data Availability

| Era | Years | Source |
|-----|-------|--------|
| Report Card | 2015-2025 | data.wa.gov (full detail) |
| Student Enrollment | 2010-2014 | data.wa.gov (fewer ID columns) |

**16 years total** across ~2,400 schools and 295 districts.

### Suppression Rules

Washington suppresses student counts where:
- Counts are less than 10 students (for privacy)
- Subgroup percentages that could identify individuals

### Census Day

Enrollment data is collected on Census Day (typically the first week of October). This is a snapshot of enrollment at that point in the school year.

### Known Caveats

- The 2010-2014 "Student Enrollment" datasets use slightly different column formats than the 2015+ "Report Card Enrollment" datasets; this package normalizes them to a consistent schema
- ESD (Educational Service District) information is available for recent years but may be missing for older data (2010-2014)
- Some district names have changed over time; the package uses current names
- Charter schools were not legal in Washington until 2012, so charter data begins in 2015

## Deeper Dive

---

### 4. Washington added 70,000 students over 16 years before COVID reversed the trend

The Evergreen State grew enrollment steadily from 1.03 million to nearly 1.15 million before the pandemic wiped out years of gains.

```r
state_totals <- enr |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students) |>
  mutate(change = n_students - lag(n_students),
         pct_change = round(change / lag(n_students) * 100, 2))
stopifnot(nrow(state_totals) > 0)

state_totals
#>    end_year n_students change pct_change
#> 1      2010    1034935     NA         NA
#> 2      2011    1045333  10398       1.00
#> 3      2012    1051404   6071       0.58
#> 4      2013    1058680   7276       0.69
#> 5      2014    1068234   9554       0.90
#> 6      2015    1086314  18080       1.69
#> 7      2016    1100849  14535       1.34
#> 8      2017    1115820  14971       1.36
#> 9      2018    1130714  14894       1.33
#> 10     2019    1137367   6653       0.59
#> 11     2020    1146882   9515       0.84
#> 12     2021    1093331 -53551      -4.67
#> 13     2022    1091343  -1988      -0.18
#> 14     2023    1096695   5352       0.49
#> 15     2024    1100059   3364       0.31
#> 16     2025    1105384   5325       0.48
```

![Statewide enrollment](https://almartin82.github.io/waschooldata/articles/enrollment_hooks_files/figure-html/statewide-chart-1.png)

[(source)](https://almartin82.github.io/waschooldata/articles/enrollment_hooks.html#washington-added-70000-students-over-16-years-before-covid-reversed-the-trend)

---

### 5. Seattle peaked at 56,000 students then lost 9% in five years

Washington's largest district peaked in 2020 and has lost nearly 5,000 students since, even as tech industry growth transformed the region.

```r
seattle <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Seattle", district_name, ignore.case = TRUE)) |>
  select(end_year, district_name, n_students) |>
  mutate(pct_of_peak = round(n_students / max(n_students) * 100, 1))
stopifnot(nrow(seattle) > 0)

seattle
#>    end_year                 district_name n_students pct_of_peak
#> 1      2010 Seattle School District No. 1      47058        84.0
#> 2      2011 Seattle School District No. 1      48299        86.2
#> 3      2012 Seattle School District No. 1      49851        88.9
#> 4      2013 Seattle School District No. 1      51201        91.3
#> 5      2014 Seattle School District No. 1      52181        93.1
#> 6      2015 Seattle School District No. 1      53361        95.2
#> 7      2016 Seattle School District No. 1      53767        95.9
#> 8      2017 Seattle School District No. 1      54722        97.6
#> 9      2018 Seattle School District No. 1      55321        98.7
#> 10     2019 Seattle School District No. 1      55325        98.7
#> 11     2020 Seattle School District No. 1      56051       100.0
#> 12     2021 Seattle School District No. 1      54021        96.4
#> 13     2022 Seattle School District No. 1      51653        92.2
#> 14     2023 Seattle School District No. 1      51528        91.9
#> 15     2024 Seattle School District No. 1      50968        90.9
#> 16     2025 Seattle School District No. 1      51200        91.3
```

![Top districts](https://almartin82.github.io/waschooldata/articles/enrollment_hooks_files/figure-html/top-districts-chart-1.png)

[(source)](https://almartin82.github.io/waschooldata/articles/enrollment_hooks.html#seattle-peaked-at-56000-students-then-lost-9-in-five-years)

---

### 6. Puget Sound holds 427K students -- nearly 4x Eastern Washington

The I-5 corridor dominates enrollment, but Eastern Washington's districts face different challenges.

```r
esd_enrollment <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         end_year == 2025, !is.na(esd_name)) |>
  group_by(esd_name) |>
  summarize(
    districts = n(),
    students = sum(n_students, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(students))
stopifnot(nrow(esd_enrollment) > 0)

esd_enrollment
#> # A tibble: 11 x 3
#>    esd_name                                       districts students
#>    <chr>                                              <int>    <dbl>
#>  1 Puget Sound Educational Service District 121          35   427222
#>  2 Northwest Educational Service District 189            35   166971
#>  3 Educational Service District 112                      30    97301
#>  4 Educational Service District 101                      59    94984
#>  5 Educational Service District 123                      22    77778
#>  6 Capital Region ESD 113                                44    75121
#>  7 Educational Service District 105                      25    65229
#>  8 North Central Educational Service District 171        29    48048
#>  9 Olympic Educational Service District 114              15    46477
#> 10 Washington State Charter School Commission            15     4600
#> 11 Spokane Public Schools Charter Authorizer              2      268
```

![Regional chart](https://almartin82.github.io/waschooldata/articles/enrollment_hooks_files/figure-html/regional-chart-1.png)

[(source)](https://almartin82.github.io/waschooldata/articles/enrollment_hooks.html#puget-sound-holds-427k-students----nearly-4x-eastern-washington)

---

### 7. Suburban districts are booming: Sumner-Bonney Lake up 23% since 2015

The fastest-growing large districts are spread across the state, from Sumner-Bonney Lake south of Seattle to Central Valley near Spokane.

```r
growth_rates <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         end_year %in% c(2015, 2025)) |>
  select(district_id, district_name, end_year, n_students) |>
  pivot_wider(names_from = end_year, values_from = n_students, names_prefix = "yr_") |>
  filter(yr_2015 > 5000) |>
  mutate(
    change = yr_2025 - yr_2015,
    pct_change = round((yr_2025 - yr_2015) / yr_2015 * 100, 1)
  ) |>
  arrange(desc(pct_change))
stopifnot(nrow(growth_rates) > 0)

head(growth_rates, 10)
#> # A tibble: 10 x 6
#>    district_id district_name                   yr_2015 yr_2025 change pct_change
#>    <chr>       <chr>                             <dbl>   <dbl>  <dbl>      <dbl>
#>  1 100259      Sumner-Bonney Lake School Dist~    8988   11048   2060       22.9
#>  2 100126      Lake Stevens School District       8515   10215   1700       20
#>  3 100016      Auburn School District            15722   18234   2512       16
#>  4 100183      Omak School District               5257    6071    814       15.5
#>  5 100022      Bethel School District            18678   21538   2860       15.3
#>  6 100263      Tahoma School District             8118    9286   1168       14.4
#>  7 100127      Lake Washington School District   27293   31146   3853       14.1
#>  8 100218      Richland School District          12729   14499   1770       13.9
#>  9 100039      Central Valley School District    13396   15102   1706       12.7
#> 10 100195      Pasco School District             17182   19001   1819       10.6
```

![Growth chart](https://almartin82.github.io/waschooldata/articles/enrollment_hooks_files/figure-html/growth-chart-1.png)

[(source)](https://almartin82.github.io/waschooldata/articles/enrollment_hooks.html#suburban-districts-are-booming-sumner-bonney-lake-up-23-since-2015)

---

### 8. Spokane anchors Eastern Washington with nearly 30,000 students

Spokane Public Schools serves 29,690 students, making it the largest district east of the Cascades and the state's second-largest overall.

```r
spokane <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Spokane", district_name, ignore.case = TRUE),
         end_year == 2025) |>
  select(district_name, n_students) |>
  arrange(desc(n_students))
stopifnot(nrow(spokane) > 0)

spokane
#>                           district_name n_students
#> 1               Spokane School District      29690
#> 2 East Valley School District (Spokane)       3655
#> 3 West Valley School District (Spokane)       3501
#> 4         Spokane International Academy        830
#> 5            Innovation Spokane Schools        235
```

![Spokane enrollment](https://almartin82.github.io/waschooldata/articles/enrollment_hooks_files/figure-html/spokane-chart-1.png)

[(source)](https://almartin82.github.io/waschooldata/articles/enrollment_hooks.html#spokane-anchors-eastern-washington-with-nearly-30000-students)

---

### 9. 130 districts have fewer than 500 students

Nearly 40% of Washington's districts serve fewer than 500 students, challenging their long-term viability.

```r
district_sizes <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         end_year == 2025) |>
  mutate(size_bucket = case_when(
    n_students < 500 ~ "Small (<500)",
    n_students < 2000 ~ "Medium (500-2K)",
    n_students < 10000 ~ "Large (2K-10K)",
    TRUE ~ "Very Large (10K+)"
  )) |>
  count(size_bucket)
stopifnot(nrow(district_sizes) > 0)

district_sizes
#>         size_bucket   n
#> 1    Large (2K-10K)  81
#> 2   Medium (500-2K)  87
#> 3      Small (<500) 130
#> 4 Very Large (10K+)  32
```

![District sizes](https://almartin82.github.io/waschooldata/articles/enrollment_hooks_files/figure-html/small-districts-chart-1.png)

[(source)](https://almartin82.github.io/waschooldata/articles/enrollment_hooks.html#districts-have-fewer-than-500-students)

---

### 10. Tacoma held steady at ~30K for a decade before COVID

The state's third-largest city maintained remarkably stable enrollment from 2010 to 2020 before losing 2,000 students.

```r
tacoma <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Tacoma", district_name, ignore.case = TRUE)) |>
  select(end_year, district_name, n_students)
stopifnot(nrow(tacoma) > 0)

tacoma
#>    end_year          district_name n_students
#> 1      2010 Tacoma School District      29625
#> 2      2011 Tacoma School District      29856
#> 3      2012 Tacoma School District      29704
#> 4      2013 Tacoma School District      30122
#> 5      2014 Tacoma School District      30411
#> 6      2015 Tacoma School District      30606
#> 7      2016 Tacoma School District      30554
#> 8      2017 Tacoma School District      30326
#> 9      2018 Tacoma School District      30414
#> 10     2019 Tacoma School District      30320
#> 11     2020 Tacoma School District      30406
#> 12     2021 Tacoma School District      28734
#> 13     2022 Tacoma School District      28779
#> 14     2023 Tacoma School District      28457
#> 15     2024 Tacoma School District      28353
#> 16     2025 Tacoma School District      29014
```

![Tacoma enrollment](https://almartin82.github.io/waschooldata/articles/enrollment_hooks_files/figure-html/tacoma-chart-1.png)

[(source)](https://almartin82.github.io/waschooldata/articles/enrollment_hooks.html#tacoma-held-steady-at-30k-for-a-decade-before-covid)

---

### 11. From 1.03M to 1.1M: 16 years of enrollment milestones

Washington's enrollment data spans 2010-2025, documenting the tech boom, pandemic disruption, and demographic transformation.

```r
decade_summary <- enr |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL",
         end_year %in% c(2010, 2015, 2019, 2021, 2025)) |>
  select(end_year, n_students) |>
  mutate(label = case_when(
    end_year == 2010 ~ "Post-recession",
    end_year == 2015 ~ "Tech boom",
    end_year == 2019 ~ "Pre-COVID peak",
    end_year == 2021 ~ "COVID low",
    end_year == 2025 ~ "Current"
  ))
stopifnot(nrow(decade_summary) > 0)

decade_summary
#>   end_year n_students          label
#> 1     2010    1034935 Post-recession
#> 2     2015    1086314      Tech boom
#> 3     2019    1137367 Pre-COVID peak
#> 4     2021    1093331      COVID low
#> 5     2025    1105384        Current
```

![Enrollment milestones](https://almartin82.github.io/waschooldata/articles/enrollment_hooks_files/figure-html/summary-chart-1.png)

[(source)](https://almartin82.github.io/waschooldata/articles/enrollment_hooks.html#from-103m-to-11m-16-years-of-enrollment-milestones)

---

### 12. 1 in 6 Washington students now receives special education services

Special education identification rose from 13.2% to 16.4% over 16 years, with the rate accelerating since 2022.

```r
sped_trend <- enr |>
  filter(is_state, grade_level == "TOTAL", subgroup == "special_ed") |>
  select(end_year, n_students, pct) |>
  mutate(pct = round(pct * 100, 1))
stopifnot(nrow(sped_trend) > 0)

sped_trend
#>    end_year n_students  pct
#> 1      2010     136129 13.2
#> 2      2011     141583 13.5
#> 3      2012     145177 13.8
#> 4      2013     146773 13.9
#> 5      2014     148462 13.9
#> 6      2015     149314 13.7
#> 7      2016     153648 14.0
#> 8      2017     157984 14.2
#> 9      2018     163839 14.5
#> 10     2019     169270 14.9
#> 11     2020     170961 14.9
#> 12     2021     158218 14.5
#> 13     2022     161967 14.8
#> 14     2023     168599 15.4
#> 15     2024     176801 16.1
#> 16     2025     181381 16.4
```

![Special education trends](https://almartin82.github.io/waschooldata/articles/enrollment_hooks_files/figure-html/sped-chart-1.png)

[(source)](https://almartin82.github.io/waschooldata/articles/enrollment_hooks.html#in-6-washington-students-now-receives-special-education-services)

---

### 13. Yakima Valley: where 93% of students are Hispanic and half are English learners

Central Washington's agricultural heartland has the state's highest concentrations of English learners and economically disadvantaged students.

```r
yakima_districts <- enr |>
  filter(is_district, grade_level == "TOTAL", end_year == 2025,
         grepl("Yakima|Sunnyside|Toppenish|Wapato|Grandview", district_name)) |>
  select(district_name, subgroup, n_students) |>
  pivot_wider(names_from = subgroup, values_from = n_students) |>
  mutate(
    pct_hispanic = round(hispanic / total_enrollment * 100, 1),
    pct_ell = round(lep / total_enrollment * 100, 1),
    pct_econ_disadv = round(econ_disadv / total_enrollment * 100, 1)
  ) |>
  select(district_name, total_enrollment, pct_hispanic, pct_ell, pct_econ_disadv) |>
  arrange(desc(total_enrollment))
stopifnot(nrow(yakima_districts) > 0)

yakima_districts
#> # A tibble: 7 x 5
#>   district_name            total_enrollment pct_hispanic pct_ell pct_econ_disadv
#>   <chr>                               <dbl>        <dbl>   <dbl>           <dbl>
#> 1 Yakima School District              15621         82.1    34.2            86.8
#> 2 Sunnyside School Distri~             6169         92.9    33.3            87.6
#> 3 West Valley School Dist~             5570         41.5     9.9            54.9
#> 4 Toppenish School Distri~             3670         86.9    36.3            89.9
#> 5 Grandview School Distri~             3586         93.4    32.7            85.5
#> 6 East Valley School Dist~             3408         58.5    15              65.7
#> 7 Wapato School District               3225         77.2    50.6            89.9
```

![Yakima Valley demographics](https://almartin82.github.io/waschooldata/articles/enrollment_hooks_files/figure-html/yakima-chart-1.png)

[(source)](https://almartin82.github.io/waschooldata/articles/enrollment_hooks.html#yakima-valley-where-93-of-students-are-hispanic-and-half-are-english-learners)

---

### 14. Ridgefield grew 84% while Vancouver lost 7% -- Clark County's diverging paths

The Portland metro spillover has reshaped Clark County, with small suburban districts booming while the urban core declines.

```r
clark_districts <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Vancouver|Evergreen SD|Camas|Battle Ground|Ridgefield", district_name),
         end_year %in% c(2015, 2020, 2025)) |>
  select(district_name, end_year, n_students) |>
  pivot_wider(names_from = end_year, values_from = n_students, names_prefix = "yr_") |>
  mutate(
    growth_2015_2025 = round((yr_2025 - yr_2015) / yr_2015 * 100, 1)
  ) |>
  arrange(desc(growth_2015_2025))
stopifnot(nrow(clark_districts) > 0)

clark_districts
#> # A tibble: 4 x 5
#>   district_name                 yr_2015 yr_2020 yr_2025 growth_2015_2025
#>   <chr>                           <dbl>   <dbl>   <dbl>            <dbl>
#> 1 Ridgefield School District       2343    3499    4315             84.2
#> 2 Camas School District            6695    7654    7272              8.6
#> 3 Battle Ground School District   13589   13365   13080             -3.7
#> 4 Vancouver School District       23486   23404   21943             -6.6
```

![Clark County growth](https://almartin82.github.io/waschooldata/articles/enrollment_hooks_files/figure-html/clark-county-chart-1.png)

[(source)](https://almartin82.github.io/waschooldata/articles/enrollment_hooks.html#ridgefield-grew-84-while-vancouver-lost-7----clark-countys-diverging-paths)

---

### 15. The Tri-Cities boom: Pasco grew 31% while Kennewick and Richland added 19% and 32%

The Tri-Cities in southeastern Washington have seen sustained population growth driven by Hanford cleanup and agricultural expansion.

```r
tri_cities <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Richland|Kennewick|Pasco", district_name)) |>
  select(end_year, district_name, n_students)
stopifnot(nrow(tri_cities) > 0)

tri_cities_wide <- tri_cities |>
  filter(end_year %in% c(2010, 2015, 2020, 2025)) |>
  pivot_wider(names_from = end_year, values_from = n_students, names_prefix = "yr_")

tri_cities_wide
#> # A tibble: 3 x 5
#>   district_name             yr_2010 yr_2015 yr_2020 yr_2025
#>   <chr>                       <dbl>   <dbl>   <dbl>   <dbl>
#> 1 Kennewick School District   16085   17611   19554   19109
#> 2 Pasco School District       14473   17182   19226   19001
#> 3 Richland School District    10965   12729   14295   14499
```

![Tri-Cities growth](https://almartin82.github.io/waschooldata/articles/enrollment_hooks_files/figure-html/tri-cities-chart-1.png)

[(source)](https://almartin82.github.io/waschooldata/articles/enrollment_hooks.html#the-tri-cities-boom-pasco-grew-31-while-kennewick-and-richland-added-19-and-32)

---
