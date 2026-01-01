# waschooldata: Fetch and Process Washington State School Data

Downloads and processes school data from the Washington Office of
Superintendent of Public Instruction (OSPI). Provides functions for
fetching enrollment data from the Washington State Report Card and
transforming it into tidy format for analysis.

## Data Source

Data is sourced from the Washington State Report Card via the
data.wa.gov open data portal (Socrata API). The Report Card provides
comprehensive enrollment data disaggregated by:

- Organization level (State, District, School)

- Grade level (PK through 12)

- Gender

- Race/Ethnicity

- Special populations (ELL, Low Income, Special Ed, etc.)

## Data Availability

- Years: 2010 to present (16 years of data)

- 2015-present: Report Card Enrollment datasets (full detail)

- 2010-2014: Student Enrollment datasets (same schema, fewer ID columns)

- Aggregation levels: State, District, School

- Update frequency: Annual (October enrollment count)

## Main Functions

- [`fetch_enr`](https://almartin82.github.io/waschooldata/reference/fetch_enr.md):
  Download enrollment data for a single year

- [`fetch_enr_multi`](https://almartin82.github.io/waschooldata/reference/fetch_enr_multi.md):
  Download enrollment data for multiple years

- [`tidy_enr`](https://almartin82.github.io/waschooldata/reference/tidy_enr.md):
  Transform wide data to long format

- [`get_available_years`](https://almartin82.github.io/waschooldata/reference/get_available_years.md):
  List available data years

## Washington Identifiers

- District Organization ID: Unique identifier for each district

- School Organization ID: Unique identifier for each school

- District Code: Short code for district identification

- School Code: Short code for school identification

## See also

Useful links:

- <https://almartin82.github.io/waschooldata/>

- <https://github.com/almartin82/waschooldata>

- Report bugs at <https://github.com/almartin82/waschooldata/issues>

## Author

**Maintainer**: Al Martin <almartin@example.com>
