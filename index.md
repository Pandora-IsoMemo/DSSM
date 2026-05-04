# DSSM - Pandora & IsoMemo spatiotemporal modeling

Shiny App for spatiotemporal modeling developed with the Pandora &
IsoMemo initiatives.

### Access to online versions:

- MAIN versions:
  - Search: <https://pandorasearch.earth/> or
    <https://isomemosearch.com/>
  - Search & Modeling: <https://isomemoapp.com/app/dssm>
- BETA versions:
  - Search: <https://isomemoapp.com/app/dssm-data-app-beta>
  - Search & Modeling: <https://isomemoapp.com/app/dssm-beta>

### Documenation

- <https://pandora-isomemo.github.io/DSSM/>

### Installation instructions

- <https://pandora-isomemo.github.io/docs/apps.html#dssm---data-search-and-spatiotemporal-modelling>

### Release notes (Changelog):

- see `NEWS.md`

### Folder for online models

- [`inst/app/predefinedModels`](https://github.com/Pandora-IsoMemo/iso-app/tree/main/inst/app/predefinedModels)

## How to use this Package

Refer to the
[vignette](https://pandora-isomemo.github.io/DSSM/articles/how-to-use-DSSM.html)
for a description of the usage of the MapR package. You can find it in
the [documentation](https://pandora-isomemo.github.io/DSSM/) of this
package.

## Notes for developers

When adding information to the *help* sites, *docstrings* or the
*vignette* of this package, please update documentation locally as
follows. The documentation of the main branch is build automatically via
github action.

``` r

devtools::document() # or CTRL + SHIFT + D in RStudio
devtools::build_site()
```

When testing with a local docker container, please make sure to rebuild
the docker image after changes in the R code or dependencies. You can do
this from the root of the repository via:

``` bash
docker build -t dssm-app:latest .
```

After that, start the container as usual via:

``` bash
docker run -p 3838:3838 dssm-app:latest
```

and access the app in your browser at `http://localhost:3838/`. Stop the
container with `CTRL + C` in the terminal.

**Optional:**

Add `-it` for interactive mode, or `--rm` to remove the container after
stopping.

## Acknowledgments

This project is licensed under the GNU General Public License v3. It
incorporates third-party packages with their respective licenses:

- **rnaturalearth** (MIT License) - Provides map data from Natural
  Earth. See
  [LICENSE.md](https://pandora-isomemo.github.io/DSSM/LICENSE.md) for
  full license details.

Interactive basemaps shown via Leaflet providers are provided by third
parties and remain subject to their own licenses, attribution
requirements, and terms of use. Users are responsible for ensuring
permitted reuse and publication of any exported or published maps. A
reference list of supported Leaflet providers is available at
[leaflet-providers
preview](https://leaflet-extras.github.io/leaflet-providers/preview/).

## Recommended Citations

If you reference methods implemented through this application in
academic work, please also cite mclust as recommended by its authors:

Scrucca L, Fraley C, Murphy TB, Raftery AE (2023). *Model-Based
Clustering, Classification, and Density Estimation Using mclust in R*.
Chapman and Hall/CRC. ISBN 978-1032234953, <doi:10.1201/9781003277965>,
<https://mclust-org.github.io/book/>.

For complete license information, please see
[LICENSE.md](https://pandora-isomemo.github.io/DSSM/LICENSE.md).
