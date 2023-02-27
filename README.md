# *trollcalibr*: a package for an easy calibration of TROLL individual-based forest simulator

[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/gsalzet/trollcalibr/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/gsalzet/trollcalibr/actions/workflows/check-standard.yaml)
[![Codecov test coverage](https://codecov.io/gh/gsalzet/trollcalibr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/gsalzet/trollcalibr?branch=main)
[![lint](https://github.com/gsalzet/trollcalibr/workflows/lint/badge.svg)](https://github.com/gsalzet/trollcalibr/actions?query=workflow%3Alint)

*trollcalibr* is a collaborative metapackage for listing all the metadata of the Species Distribution Modelling (SDM) packages.
*trollcalibr* includes a graphical interface available locally or online (see below).
*trollcalibr* integrates metadata from SDM packages and checks their validity against the CRAN metadata for perenity.
If you want to contribute with your package metadata, please use the contribution guidelines (see below).

## Installation

You can install the latest version of **trollcalibr** from Github using the [`devtools`](https://github.com/r-lib/devtools) package:

``` r
if (!requireNamespace("devtools", quietly = TRUE))
  install.packages("devtools")

devtools::install_github("gsalzet/trollcalibr")
```

## Usage


## Contribution

If you want to contribute with your package metadata, please use a pull request following [the contribution guidelines](https://github.com/gsalzet/trollcalibr/blob/main/CONTRIBUTING.md).