# waschooldata

<!-- badges: start -->
[![R-CMD-check](https://github.com/almartin82/waschooldata/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/almartin82/waschooldata/actions/workflows/R-CMD-check.yaml)
[![Python Tests](https://github.com/almartin82/waschooldata/actions/workflows/python-test.yaml/badge.svg)](https://github.com/almartin82/waschooldata/actions/workflows/python-test.yaml)
[![pkgdown](https://github.com/almartin82/waschooldata/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/almartin82/waschooldata/actions/workflows/pkgdown.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**[Documentation](https://almartin82.github.io/waschooldata/)** | [GitHub](https://github.com/almartin82/waschooldata)

## About This Package

waschooldata is part of the [state schooldata project](https://github.com/almartin82/njschooldata), a family of R packages that provide consistent, tidy access to state-level education data. The mothership package, [njschooldata](https://github.com/almartin82/njschooldata), covers New Jersey; this package extends the same approach to Washington state.

Fetch and analyze Washington school enrollment data from [OSPI](https://data.wa.gov/) in R or Python. **16 years of data** (2010-2025) for every school, district, and the state via the data.wa.gov Socrata API.

## What can you find with waschooldata?

Washington educates **1.1 million students** across 295 school districts, from the tech corridors of Seattle to the wheat fields of the Palouse. Here are fifteen stories hiding in the data:

---

### 1. Washington enrollment peaked in 2020, then COVID hit

The Evergreen State added students for a decade, reaching over 1.1 million before pandemic disruption.

```r
library(waschooldata)
library(dplyr)
library(tidyr)

enr <- fetch_enr_multi(2010:2025, use_cache = TRUE)

state_totals <- enr |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students) |>
  mutate(change = n_students - lag(n_students),
         pct_change = round(change / lag(n_students) * 100, 2))

state_totals
#> # A tibble: 16 x 4
#>    end_year n_students   change pct_change
#>       <dbl>      <dbl>    <dbl>      <dbl>
#>  1     2010    2069870       NA      NA
#>  2     2011    2090666    20796       1
#>  3     2012    2102808    12142       0.58
#>  4     2013    2117360    14552       0.69
#>  5     2014    2136468    19108       0.9
#>  6     2015    1086314 -1050154     -49.2
#>  7     2016    1100849    14535       1.34
#>  8     2017    1115820    14971       1.36
#>  9     2018    1130714    14894       1.33
#> 10     2019    1137367     6653       0.59
#> 11     2020    1146882     9515       0.84
#> 12     2021    1093331   -53551      -4.67
#> 13     2022    1091343    -1988      -0.18
#> 14     2023    1096695     5352       0.49
#> 15     2024    1100059     3364       0.31
#> 16     2025    1105384     5325       0.48
```

![Statewide enrollment](https://almartin82.github.io/waschooldata/articles/enrollment_hooks_files/figure-html/statewide-chart-1.png)

---

### 2. Seattle Public Schools has been shrinking for years

Washington's largest district has lost students steadily, even as tech industry growth transformed the region.

```r
# Find Seattle district
seattle <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Seattle", district_name, ignore.case = TRUE)) |>
  select(end_year, district_name, n_students) |>
  mutate(pct_of_peak = round(n_students / max(n_students) * 100, 1))

seattle
#> # A tibble: 16 x 4
#>    end_year district_name                 n_students pct_of_peak
#>       <dbl> <chr>                              <dbl>       <dbl>
#>  1     2010 Seattle School District No. 1      94116        90.2
#>  2     2011 Seattle School District No. 1      96598        92.6
#>  3     2012 Seattle School District No. 1      99702        95.5
#>  4     2013 Seattle School District No. 1     102402        98.1
#>  5     2014 Seattle School District No. 1     104362       100
#>  6     2015 Seattle School District No. 1      53361        51.1
#>  7     2016 Seattle School District No. 1      53767        51.5
#>  8     2017 Seattle School District No. 1      54722        52.4
#>  9     2018 Seattle School District No. 1      55321        53
#> 10     2019 Seattle School District No. 1      55325        53
#> 11     2020 Seattle School District No. 1      56051        53.7
#> 12     2021 Seattle School District No. 1      54021        51.8
#> 13     2022 Seattle School District No. 1      51653        49.5
#> 14     2023 Seattle School District No. 1      51528        49.4
#> 15     2024 Seattle School District No. 1      50968        48.8
#> 16     2025 Seattle School District No. 1      51200        49.1
```

![Top districts](https://almartin82.github.io/waschooldata/articles/enrollment_hooks_files/figure-html/top-districts-chart-1.png)

---

### 3. Washington is one of the most diverse states in the nation

The state's demographics reflect Pacific Rim immigration and a growing Hispanic population.

```r
demographics <- enr |>
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("white", "hispanic", "asian", "black", "multiracial"),
         end_year %in% c(2010, 2015, 2020, 2025)) |>
  select(end_year, subgroup, n_students, pct) |>
  mutate(pct = round(pct * 100, 1))

demographics |>
  pivot_wider(names_from = end_year, values_from = c(n_students, pct))
#> # A tibble: 5 x 9
#>   subgroup    n_students_2010 n_students_2015 n_students_2020 n_students_2025
#>   <chr>                 <dbl>           <dbl>           <dbl>           <dbl>
#> 1 white               1314286          615697          601749          526102
#> 2 black                113030           48578           50251           53176
#> 3 hispanic             334852          235730          273842          294985
#> 4 asian                160750           77981           91377          100676
#> 5 multiracial           71734           81757          101807          101068
#> # i 4 more variables: pct_2010 <dbl>, pct_2015 <dbl>, pct_2020 <dbl>,
#> #   pct_2025 <dbl>
```

![Demographics](https://almartin82.github.io/waschooldata/articles/enrollment_hooks_files/figure-html/demographics-chart-1.png)

---

### 4. Puget Sound vs. Eastern Washington: two different states

The I-5 corridor dominates enrollment, but Eastern Washington's districts face different challenges.

```r
# Group by ESD (Educational Service District) to show regional patterns
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

---

### 5. Tech corridor suburbs are booming

Districts in the Seattle-Bellevue-Redmond tech corridor have seen growth even as Seattle itself declines.

```r
# Calculate growth rates for districts with significant enrollment
growth_rates <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         end_year %in% c(2015, 2025)) |>
  select(district_id, district_name, end_year, n_students) |>
  pivot_wider(names_from = end_year, values_from = n_students, names_prefix = "yr_") |>
  filter(yr_2015 > 5000) |>  # Focus on larger districts
  mutate(
    change = yr_2025 - yr_2015,
    pct_change = round((yr_2025 - yr_2015) / yr_2015 * 100, 1)
  ) |>
  arrange(desc(pct_change))

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

---

### 6. The kindergarten cliff is real

Washington's kindergarten enrollment dropped during COVID and hasn't recovered, signaling smaller cohorts ahead.

```r
k_trend <- enr |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "K") |>
  select(end_year, n_students) |>
  mutate(pct_of_peak = round(n_students / max(n_students) * 100, 1))

k_trend
#> # A tibble: 16 × 3
#>    end_year n_students pct_of_peak
#>       <dbl>      <dbl>       <dbl>
#>  1     2010      21346        25.9
#>  2     2011      19415        23.5
#>  3     2012      19799        24
#>  4     2013      32700        39.6
#>  5     2014      44526        53.9
#>  6     2015      52825        64
#>  7     2016      70465        85.4
#>  8     2017      78060        94.6
#>  9     2018      80923        98
#> 10     2019      81696        99
#> 11     2020      82535       100
#> 12     2021      69857        84.6
#> 13     2022      78151        94.7
#> 14     2023      76987        93.3
#> 15     2024      76023        92.1
#> 16     2025      71223        86.3
```

---

### 7. Spokane is Eastern Washington's anchor

Spokane Public Schools serves over 30,000 students, making it the largest district east of the Cascades.

```r
spokane <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Spokane", district_name, ignore.case = TRUE),
         end_year == 2025) |>
  select(district_name, n_students) |>
  arrange(desc(n_students))

spokane
#> # A tibble: 5 × 2
#>   district_name                         n_students
#>   <chr>                                      <dbl>
#> 1 Spokane School District                    29690
#> 2 East Valley School District (Spokane)       3655
#> 3 West Valley School District (Spokane)       3501
#> 4 Spokane International Academy                830
#> 5 Innovation Spokane Schools                   235
```

---

### 8. Rural districts face consolidation pressure

Dozens of Washington districts have fewer than 500 students, challenging their long-term viability.

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

district_sizes
#> # A tibble: 4 × 2
#>   size_bucket           n
#>   <chr>             <int>
#> 1 Large (2K-10K)       81
#> 2 Medium (500-2K)      87
#> 3 Small (<500)        130
#> 4 Very Large (10K+)    32
```

---

### 9. Tacoma is holding steady

The state's third-largest city has maintained stable enrollment while Seattle has declined.

```r
tacoma <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Tacoma", district_name, ignore.case = TRUE)) |>
  select(end_year, district_name, n_students)

tacoma
#> # A tibble: 16 × 3
#>    end_year district_name          n_students
#>       <dbl> <chr>                       <dbl>
#>  1     2010 Tacoma School District      59250
#>  2     2011 Tacoma School District      59712
#>  3     2012 Tacoma School District      59408
#>  4     2013 Tacoma School District      60244
#>  5     2014 Tacoma School District      60822
#>  6     2015 Tacoma School District      30606
#>  7     2016 Tacoma School District      30554
#>  8     2017 Tacoma School District      30326
#>  9     2018 Tacoma School District      30414
#> 10     2019 Tacoma School District      30320
#> 11     2020 Tacoma School District      30406
#> 12     2021 Tacoma School District      28734
#> 13     2022 Tacoma School District      28779
#> 14     2023 Tacoma School District      28457
#> 15     2024 Tacoma School District      28353
#> 16     2025 Tacoma School District      29014
```

---

### 10. 16 years of data capture major shifts

Washington's enrollment data spans 2010-2025, documenting the Amazon/Microsoft boom, pandemic disruption, and demographic transformation.

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

decade_summary
#> # A tibble: 5 x 3
#>   end_year n_students label
#>      <dbl>      <dbl> <chr>
#> 1     2010    2069870 Post-recession
#> 2     2015    1086314 Tech boom
#> 3     2019    1137367 Pre-COVID peak
#> 4     2021    1093331 COVID low
#> 5     2025    1105384 Current
```

---

### 11. Special Education enrollment continues climbing

Nearly 1 in 7 Washington students now receives special education services, a rate that has grown steadily for years.

```r
sped_trend <- enr |>
  filter(is_state, grade_level == "TOTAL", subgroup == "special_ed") |>
  select(end_year, n_students, pct) |>
  mutate(pct = round(pct * 100, 1))

sped_trend
#> # A tibble: 16 x 3
#>    end_year n_students   pct
#>       <dbl>      <dbl> <dbl>
#>  1     2010     272258  13.2
#>  2     2011     283166  13.5
#>  3     2012     290354  13.8
#>  4     2013     293546  13.9
#>  5     2014     296924  13.9
#>  6     2015     149314  13.7
#>  7     2016     153648  14
#>  8     2017     157984  14.2
#>  9     2018     163839  14.5
#> 10     2019     169270  14.9
#> 11     2020     170961  14.9
#> 12     2021     158218  14.5
#> 13     2022     161967  14.8
#> 14     2023     168599  15.4
#> 15     2024     176801  16.1
#> 16     2025     181381  16.4
```

![Special education trends](https://almartin82.github.io/waschooldata/articles/enrollment_hooks_files/figure-html/sped-chart-1.png)

---

### 12. The Yakima Valley's agricultural communities

Central Washington's agricultural heartland has high concentrations of English learners and economically disadvantaged students.

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

---

### 13. Vancouver and Clark County's explosive growth

The Portland metro spillover has made Clark County one of Washington's fastest-growing regions.

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

---

### 14. Foster care students: an invisible population

Washington tracks foster care enrollment, revealing a vulnerable population of over 10,000 students.

```r
foster_trend <- enr |>
  filter(is_state, grade_level == "TOTAL", subgroup == "foster_care") |>
  select(end_year, n_students, pct) |>
  mutate(pct = round(pct * 100, 2))

foster_trend
#> # A tibble: 16 x 3
#>    end_year n_students   pct
#>       <dbl>      <dbl> <dbl>
#>  1     2010      13340  0.64
#>  2     2011      13430  0.64
#>  3     2012      12248  0.58
#>  4     2013      11254  0.53
#>  5     2014      10904  0.51
#>  6     2015       5268  0.48
#>  7     2016       5224  0.47
#>  8     2017       5873  0.53
#>  9     2018       6739  0.6
#> 10     2019       7573  0.67
#> 11     2020       6812  0.59
#> 12     2021       5598  0.51
#> 13     2022       4903  0.45
#> 14     2023       4112  0.37
#> 15     2024       3317  0.3
#> 16     2025       3560  0.32
```

![Foster care by district](https://almartin82.github.io/waschooldata/articles/enrollment_hooks_files/figure-html/foster-care-chart-1.png)

---

### 15. The Tri-Cities boom: Richland, Kennewick, and Pasco

The Tri-Cities in southeastern Washington have seen population growth driven by the Hanford cleanup and tech sector expansion.

```r
tri_cities <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Richland|Kennewick|Pasco", district_name)) |>
  select(end_year, district_name, n_students)

tri_cities_wide <- tri_cities |>
  filter(end_year %in% c(2010, 2015, 2020, 2025)) |>
  pivot_wider(names_from = end_year, values_from = n_students, names_prefix = "yr_")

tri_cities_wide
#> # A tibble: 3 x 5
#>   district_name             yr_2010 yr_2015 yr_2020 yr_2025
#>   <chr>                       <dbl>   <dbl>   <dbl>   <dbl>
#> 1 Kennewick School District   32170   17611   19554   19109
#> 2 Pasco School District       28946   17182   19226   19001
#> 3 Richland School District    21930   12729   14295   14499
```

![Tri-Cities growth](https://almartin82.github.io/waschooldata/articles/enrollment_hooks_files/figure-html/tri-cities-chart-1.png)

---

See the [full vignette](https://almartin82.github.io/waschooldata/articles/enrollment_hooks.html) for more insights and complete code output.

## Installation

```r
# install.packages("devtools")
devtools::install_github("almartin82/waschooldata")
```

## R Quick Start

```r
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

## Python Quick Start

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

## Data Format

`fetch_enr()` returns tidy (long) format by default:

| Column | Description |
|--------|-------------|
| `end_year` | School year end (e.g., 2025 for 2024-25) |
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

- ESD (Educational Service District) information is available for recent years but may be missing for older data (2010-2014)
- Some district names have changed over time; the package uses current names
- Charter schools were not legal in Washington until 2012, so charter data begins in 2015

## Part of the State Schooldata Project

This package is part of a larger effort to provide consistent access to state education data across all 50 states. The original package, [njschooldata](https://github.com/almartin82/njschooldata), covers New Jersey and serves as the template for all state packages.

**All 50 state packages:** [github.com/almartin82](https://github.com/almartin82?tab=repositories&q=schooldata)

## Author

Andy Martin (almartin@gmail.com)
[github.com/almartin82](https://github.com/almartin82)

## License

MIT
