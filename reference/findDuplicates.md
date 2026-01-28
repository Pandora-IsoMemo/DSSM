# Finds duplicates in data using user specified similarity rules

Finds duplicates in data using user specified similarity rules

## Usage

``` r
findDuplicates(data, userSimilaritySelection, addColumn, keepFirst)
```

## Arguments

- data:

  dataframe in which duplicates are searched for

- userSimilaritySelection:

  dataframe containing similarity rules for each column to be considered

- addColumn:

  logical should a column with duplicate row indices be added

- keepFirst:

  logical keep first (TRUE) or last (FALSE) duplicate row
