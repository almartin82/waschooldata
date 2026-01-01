# 10 Insights from Washington School Enrollment Data

``` r
library(waschooldata)
library(dplyr)
library(tidyr)
library(ggplot2)

theme_set(theme_minimal(base_size = 14))
```

This vignette explores Washington’s public school enrollment data,
surfacing key trends across 16 years of data (2010-2025).

------------------------------------------------------------------------

## 1. Washington enrollment peaked in 2020, then COVID hit

The Evergreen State added students for a decade, reaching over 1.1
million before pandemic disruption.

``` r
enr <- fetch_enr_multi(2010:2025)

state_totals <- enr |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students) |>
  mutate(change = n_students - lag(n_students),
         pct_change = round(change / lag(n_students) * 100, 2))

state_totals
```

``` r
ggplot(state_totals, aes(x = end_year, y = n_students)) +
  geom_line(linewidth = 1.2, color = "#4B2E83") +
  geom_point(size = 3, color = "#4B2E83") +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "red", alpha = 0.5) +
  annotate("text", x = 2020.5, y = max(state_totals$n_students, na.rm = TRUE),
           label = "COVID", hjust = 0, color = "red", size = 3) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Washington Public School Enrollment (2010-2025)",
    subtitle = "A decade of growth disrupted by the pandemic",
    x = "School Year (ending)",
    y = "Total Enrollment"
  )
```

------------------------------------------------------------------------

## 2. Seattle Public Schools has been shrinking for years

Washington’s largest district has lost students steadily, even as tech
industry growth transformed the region.

``` r
# Find Seattle district
seattle <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Seattle", district_name, ignore.case = TRUE)) |>
  select(end_year, district_name, n_students) |>
  mutate(pct_of_peak = round(n_students / max(n_students) * 100, 1))

seattle
```

``` r
top_districts <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         end_year == 2025) |>
  arrange(desc(n_students)) |>
  head(5) |>
  pull(district_id)

enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         district_id %in% top_districts) |>
  ggplot(aes(x = end_year, y = n_students, color = district_name)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Washington's Top 5 Districts: Enrollment Trends",
    subtitle = "Seattle shrinks while Spokane and suburban districts hold steady",
    x = "School Year",
    y = "Enrollment",
    color = "District"
  ) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2))
```

------------------------------------------------------------------------

## 3. Washington is one of the most diverse states in the nation

The state’s demographics reflect Pacific Rim immigration and a growing
Hispanic population.

``` r
demographics <- enr |>
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("white", "hispanic", "asian", "black", "multiracial"),
         end_year %in% c(2010, 2015, 2020, 2025)) |>
  select(end_year, subgroup, n_students, pct) |>
  mutate(pct = round(pct * 100, 1))

demographics |>
  pivot_wider(names_from = end_year, values_from = c(n_students, pct))
```

``` r
enr |>
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("white", "hispanic", "asian", "black", "multiracial")) |>
  ggplot(aes(x = end_year, y = pct * 100, color = subgroup)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_color_brewer(palette = "Set2",
                     labels = c("Asian", "Black", "Hispanic", "Multiracial", "White")) +
  labs(
    title = "Washington's Shifting Demographics (2010-2025)",
    subtitle = "White enrollment declining as Hispanic and multiracial populations grow",
    x = "School Year",
    y = "Percent of Students",
    color = "Race/Ethnicity"
  ) +
  theme(legend.position = "bottom")
```

------------------------------------------------------------------------

## 4. Puget Sound vs. Eastern Washington: two different states

The I-5 corridor dominates enrollment, but Eastern Washington’s
districts face different challenges.

``` r
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
```

``` r
esd_enrollment |>
  mutate(region = case_when(
    grepl("Puget Sound", esd_name, ignore.case = TRUE) ~ "Puget Sound",
    grepl("Seattle|King", esd_name, ignore.case = TRUE) ~ "Puget Sound",
    grepl("Pierce|Tacoma", esd_name, ignore.case = TRUE) ~ "Puget Sound",
    grepl("Spokane", esd_name, ignore.case = TRUE) ~ "Eastern WA",
    grepl("ESD 101|ESD 105|ESD 123", esd_name) ~ "Eastern WA",
    TRUE ~ "Other"
  )) |>
  group_by(region) |>
  summarize(students = sum(students), .groups = "drop") |>
  ggplot(aes(x = reorder(region, students), y = students, fill = region)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = scales::comma(students)), hjust = -0.1) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c("Puget Sound" = "#4B2E83", "Eastern WA" = "#E8E3D3", "Other" = "#85754D")) +
  labs(
    title = "Washington Enrollment by Region",
    subtitle = "Puget Sound dominates the state's student population",
    x = NULL,
    y = "Total Students"
  )
```

------------------------------------------------------------------------

## 5. Tech corridor suburbs are booming

Districts in the Seattle-Bellevue-Redmond tech corridor have seen growth
even as Seattle itself declines.

``` r
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
```

``` r
growth_rates |>
  head(10) |>
  mutate(district_name = forcats::fct_reorder(district_name, pct_change)) |>
  ggplot(aes(x = pct_change, y = district_name, fill = pct_change > 0)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(ifelse(pct_change > 0, "+", ""), pct_change, "%")),
            hjust = ifelse(growth_rates$pct_change[1:10] > 0, -0.1, 1.1), size = 3) +
  scale_x_continuous(expand = expansion(mult = c(0.1, 0.15))) +
  scale_fill_manual(values = c("TRUE" = "#4B2E83", "FALSE" = "#C4A055")) +
  labs(
    title = "Fastest Growing Large Districts (2015-2025)",
    subtitle = "Among districts with 5,000+ students in 2015",
    x = "Percent Change",
    y = NULL
  )
```

------------------------------------------------------------------------

## 6. The kindergarten cliff is real

Washington’s kindergarten enrollment dropped during COVID and hasn’t
recovered, signaling smaller cohorts ahead.

``` r
k_trend <- enr |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "K") |>
  select(end_year, n_students) |>
  mutate(pct_of_peak = round(n_students / max(n_students) * 100, 1))

k_trend
```

------------------------------------------------------------------------

## 7. Spokane is Eastern Washington’s anchor

Spokane Public Schools serves over 30,000 students, making it the
largest district east of the Cascades.

``` r
spokane <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Spokane", district_name, ignore.case = TRUE),
         end_year == 2025) |>
  select(district_name, n_students) |>
  arrange(desc(n_students))

spokane
```

------------------------------------------------------------------------

## 8. Rural districts face consolidation pressure

Dozens of Washington districts have fewer than 500 students, challenging
their long-term viability.

``` r
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
```

------------------------------------------------------------------------

## 9. Tacoma is holding steady

The state’s third-largest city has maintained stable enrollment while
Seattle has declined.

``` r
tacoma <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Tacoma", district_name, ignore.case = TRUE)) |>
  select(end_year, district_name, n_students)

tacoma
```

------------------------------------------------------------------------

## 10. 16 years of data capture major shifts

Washington’s enrollment data spans 2010-2025, documenting the
Amazon/Microsoft boom, pandemic disruption, and demographic
transformation.

``` r
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
```

------------------------------------------------------------------------

## Summary

Washington’s school enrollment data reveals:

- **Tech corridor paradox**: Seattle shrinks while suburban Eastside
  districts grow
- **Demographic diversity**: One of the nation’s most diverse student
  populations
- **Regional divide**: Puget Sound vs. Eastern Washington face different
  challenges
- **Kindergarten cliff**: COVID-era enrollment drops rippling through
  the system
- **Rural pressure**: Many small districts below sustainable enrollment
  levels

These patterns shape school funding, facility planning, and staffing
decisions across the Evergreen State.

------------------------------------------------------------------------

*Data sourced from the Washington Office of Superintendent of Public
Instruction (OSPI) via the [Washington State Report
Card](https://washingtonstatereportcard.ospi.k12.wa.us/) and
[data.wa.gov](https://data.wa.gov/).*
