# Extract XPred Center

Extract XPred Center

## Usage

``` r
extractXPredCenter(
  XPred,
  centerX,
  centerY,
  Radius,
  batch = FALSE,
  isThreeD = FALSE,
  data = NULL,
  time = NULL
)
```

## Arguments

- XPred:

  (data.frame) data.frame containing all estimates

- centerX:

  (numeric) center for longitude

- centerY:

  (numeric) center for latitude

- Radius:

  (numeric) radius of the area around the center

- batch:

  (logical) set TRUE if used for batch estimates

- isThreeD:

  (logical) set TRUE if used in plotMap3D

- data:

  (data.frame) data.frame with input data for the model

- time:

  (numeric) center of time
