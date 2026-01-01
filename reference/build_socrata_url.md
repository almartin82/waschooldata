# Build Socrata API URL with filters

Constructs a SODA API URL with optional filters.

## Usage

``` r
build_socrata_url(dataset_id, filters = NULL, select = NULL, limit = 50000)
```

## Arguments

- dataset_id:

  The dataset identifier

- filters:

  Named list of column=value filters

- select:

  Character vector of columns to select (NULL for all)

- limit:

  Maximum records to return

## Value

Character URL
