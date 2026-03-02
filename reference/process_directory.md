# Process directory data into standardized schema

Combines entity data from the enrollment API with district contact
information from the EDS directory into a clean, standardized format.

## Usage

``` r
process_directory(entity_data, eds_data, end_year)
```

## Arguments

- entity_data:

  Data frame from get_directory_entities()

- eds_data:

  Data frame from get_eds_directory(), or NULL

- end_year:

  School year end

## Value

Processed data frame with standard directory schema
