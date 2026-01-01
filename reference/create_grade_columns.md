# Create grade-level columns from grade rows

Pivots grade-specific enrollment data to wide format with grade_pk,
grade_k, grade_01, etc. columns.

## Usage

``` r
create_grade_columns(grade_df, key_cols)
```

## Arguments

- grade_df:

  Data frame with grade-level rows

- key_cols:

  Character vector of key columns for joining

## Value

Data frame with grade columns, or NULL if no data
