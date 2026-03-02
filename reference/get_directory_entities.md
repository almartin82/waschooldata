# Get directory entities from enrollment API

Extracts unique school and district entities from the enrollment data on
data.wa.gov. This provides the core directory information: names, codes,
types, counties, and ESD assignments.

## Usage

``` r
get_directory_entities(end_year)
```

## Arguments

- end_year:

  School year end

## Value

Data frame with unique entity records
