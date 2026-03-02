# Fetch Washington school directory data

Downloads and processes school and district directory data from the
Washington OSPI. School/district entity data comes from the Report Card
enrollment dataset on data.wa.gov. District contact information
(address, phone, email, administrator) comes from the OSPI Education
Directory System.

## Usage

``` r
fetch_directory(end_year = NULL, tidy = TRUE, use_cache = TRUE)
```

## Arguments

- end_year:

  School year end to use for entity data. Defaults to the most recent
  available year. Year is the end of the academic year, e.g. 2025 for
  the 2024-25 school year.

- tidy:

  If TRUE (default), returns data in a standardized format with
  consistent column names. If FALSE, returns with minimal processing.

- use_cache:

  If TRUE (default), uses locally cached data when available. Set to
  FALSE to force re-download from OSPI.

## Value

A data frame with directory data. Key columns include:

- entity_type:

  "State", "District", or "School"

- state_district_id:

  OSPI district organization ID

- district_code:

  Short district code

- district_name:

  District name

- state_school_id:

  OSPI school organization ID (schools only)

- school_code:

  Short school code (schools only)

- school_name:

  School name (schools only)

- school_type:

  School type code (P=Public, S=Special, etc.)

- county_name:

  County

- esd_name:

  Educational Service District name

- esd_id:

  ESD organization ID

- address:

  Street address (districts only, from EDS)

- city:

  City (districts only)

- state:

  State (always "WA")

- zip:

  ZIP code (districts only, from EDS)

- administrator_name:

  Superintendent name (districts only, from EDS)

- phone:

  Phone number (districts only, from EDS)

- email:

  Email address (districts only, from EDS)

## Examples

``` r
if (FALSE) { # \dontrun{
# Get directory data
dir_data <- fetch_directory()

# Get raw format
dir_raw <- fetch_directory(tidy = FALSE)

# Force fresh download
dir_fresh <- fetch_directory(use_cache = FALSE)

# Filter to schools only
library(dplyr)
schools <- dir_data |>
  filter(entity_type == "School")

# Find all schools in Seattle
seattle <- dir_data |>
  filter(district_name == "Seattle School District No. 1",
         entity_type == "School")

# Districts with contact info
districts <- dir_data |>
  filter(entity_type == "District", !is.na(phone))
} # }
```
