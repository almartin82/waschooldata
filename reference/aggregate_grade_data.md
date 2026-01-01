# Aggregate grade-level data to organization level

Washington data has separate rows for each grade within an organization.
This function creates organization-level summary rows with grade
breakdowns.

## Usage

``` r
aggregate_grade_data(df, end_year)
```

## Arguments

- df:

  Processed data frame with grade-level rows

- end_year:

  School year end

## Value

Data frame with aggregated rows
