# Select Cluster Grouping

Select Cluster Grouping

## Usage

``` r
selectClusterGrouping(data, cluster, clusterResults)
```

## Arguments

- data:

  (data.frame) data if `cluster == TRUE` it must contain columns
  "spatial_cluster" and "temporal_group"

- cluster:

  (logical) if TRUE, a column "cluster" is added to data

- clusterResults:

  (integer) either `0` or `1`, using "temporal_group" or
  "spatial_cluster" for the column "cluster", respectively.

## Value

(data.frame) data containing the column "cluster" if `cluster == TRUE`,
else just the input `data`
