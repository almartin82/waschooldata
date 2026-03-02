# Merge EDS contact data into tidy directory

Joins district contact information from the EDS directory into the
processed directory data. Matching is done by district name since the
EDS page does not include OSPI organization IDs.

## Usage

``` r
merge_eds_contact_tidy(result, eds_data)
```

## Arguments

- result:

  Processed directory data frame

- eds_data:

  EDS directory data frame

## Value

Updated data frame with contact columns
