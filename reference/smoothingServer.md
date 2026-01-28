# Smoothing Server

Smoothing Server

## Usage

``` r
smoothingServer(
  input,
  output,
  session,
  ns,
  title = "Number of Basis Functions (Smoothing)",
  map3D = FALSE,
  label = "No. of time basis functions"
)
```

## Arguments

- input:

  shiny input

- output:

  shiny output

- session:

  shiny session

- ns:

  namespace

- title:

  title of the modal

- map3D:

  logical, if TRUE, the map is 3D (contains the time dimension)

- label:

  if \`map3D == TRUE\` label of the slider for the number of time basis
  functions
