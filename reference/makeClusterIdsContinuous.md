# Make cluster ids continuous

When Mclust is used with e.g. 10 clusters, it can still happen that some
clusters are empty In this case we would see a jump in cluster ids e.g.
1,2,5,... To prevent this, we change the cluster ids in the last step.

## Usage

``` r
makeClusterIdsContinuous(column_with_ids)
```

## Arguments

- column_with_ids:

  A vector with cluster ids
