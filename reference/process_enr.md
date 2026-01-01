# Process raw OSPI enrollment data

Transforms raw Report Card data into a standardized schema with state,
district, and school level aggregations.

## Usage

``` r
process_enr(raw_data, end_year)
```

## Arguments

- raw_data:

  Data frame from get_raw_enr

- end_year:

  School year end

## Value

Processed data frame with standardized columns
