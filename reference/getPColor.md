# Get Point Color

Get Point Color

## Usage

``` r
getPColor(data, cluster, palName, pColor)
```

## Arguments

- data:

  (data.frame) if cluster, different ids in the column "cluster" will
  receive a different colour

- cluster:

  (logical) if TRUE, return color for cluster

- palName:

  (character) name of the color palette

- pColor:

  single color or vector of colors that is returned if
  `cluster == FALSE`

## Value

single color or vector of colors
