# Get available years for Washington enrollment data

Returns a vector of end years for which enrollment data is available
from the Washington State Report Card data portal.

## Usage

``` r
get_available_years()
```

## Value

Integer vector of available end years

## Details

Data Eras:

- Era 1 (2010-2024): Modern schema via data.wa.gov Socrata API

  - 2010-2014: "Student Enrollment" datasets (same schema)

  - 2015-2024: "Report Card Enrollment" datasets

- Era 2 (1994-2001): Legacy schema with different column structure (not
  currently supported - requires separate processing)

## Examples

``` r
if (FALSE) { # \dontrun{
available <- get_available_years()
print(available)
} # }
```
