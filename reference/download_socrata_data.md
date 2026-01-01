# Download data from Socrata API

Downloads data from data.wa.gov using the Socrata Open Data API. Handles
pagination for large datasets.

## Usage

``` r
download_socrata_data(dataset_id, end_year)
```

## Arguments

- dataset_id:

  The 4-4 alphanumeric dataset identifier

- end_year:

  School year end (for error messages)

## Value

Data frame with all records
