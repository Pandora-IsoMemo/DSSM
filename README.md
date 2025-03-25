# DSSM - Pandora & IsoMemo spatiotemporal modeling

<!-- badges: start -->
[![R-CMD-check](https://github.com/Pandora-IsoMemo/DSSM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Pandora-IsoMemo/DSSM/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Shiny App for spatiotemporal modeling developed with the Pandora & IsoMemo initiatives.

### Access to online versions:

- MAIN versions: 
  - Search: https://pandorasearch.earth/ or https://isomemosearch.com/
  - Search & Modeling: https://isomemoapp.com/app/dssm
- BETA versions:
  - Search: https://isomemoapp.com/app/dssm-data-app-beta
  - Search & Modeling: https://isomemoapp.com/app/dssm-beta

### Documenation

- https://pandora-isomemo.github.io/DSSM/

### Installation instructions

- https://pandora-isomemo.github.io/docs/apps.html#dssm---data-search-and-spatiotemporal-modelling

### Release notes (Changelog):

- see `NEWS.md`

### Folder for online models

- [`inst/app/predefinedModels`](https://github.com/Pandora-IsoMemo/iso-app/tree/main/inst/app/predefinedModels)

## How to use this Package

Refer to the [vignette](https://pandora-isomemo.github.io/DSSM/articles/how-to-use-DSSM.html) 
for a description of the usage of the MapR package. You can find it in the 
[documentation](https://pandora-isomemo.github.io/DSSM/) of this package.

## Notes for developers

When adding information to the _help_ sites, _docstrings_ or the _vignette_ of this 
package, please update documentation locally as follows. The documentation of
the main branch is build automatically via github action.

```R
devtools::document() # or CTRL + SHIFT + D in RStudio
devtools::build_site()
```
