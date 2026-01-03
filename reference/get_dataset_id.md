# Get dataset ID for a given school year

Returns the Socrata dataset ID for the enrollment data for a given
school year end.

## Usage

``` r
get_dataset_id(end_year)
```

## Arguments

- end_year:

  School year end (e.g., 2024 for 2023-24 school year)

## Value

Character string with dataset ID, or NULL if not available

## Details

Data sources by era:

- 2015-2024: "Report Card Enrollment" datasets

- 2010-2014: "Student Enrollment" datasets (same schema)
