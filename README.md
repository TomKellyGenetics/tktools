# tktools

## Version 0.1.0

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/tktools)](https://cran.r-project.org/package=tktools)
[![Travis Build Status](https://travis-ci.org/TomKellyGenetics/tktools.svg?branch=master)](https://travis-ci.org/TomKellyGenetics/tktools)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/TomKellyGenetics/vioplot?branch=master&svg=true)](https://ci.appveyor.com/project/TomKellyGenetics/tktools)
[![Project Status: WIP – Some functions have been added and documented but more will continue to be developed.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![codecov](https://codecov.io/gh/TomKellyGenetics/tktools/branch/master/graph/badge.svg)](https://codecov.io/gh/TomKellyGenetics/tktools)

### Tom Kelly's R tools

Personal Repository of miscellaneous functions, tools, tweaks, aliases, and settings. R functions documented in package. 

This package contains various utilities to manipute (genomic) data and tweak plots in R. Functions will be added as needed for various projects if they could be used more widely. This repository is mainly intended for personal use (but issues and PRs are still welcome) so functions may be poorly documented or lack unit tests. Many are a work in progress. There is no plan to submit this to CRAN or other package repositories.

## Installation

To get the development version from github:

```R
# install.packages("devtools")
devtools::install_github("TomKellyGenetics/tktools")
```

## Running

There are no currently vignettes demonstrating these functions but each has been documented.

## Functionality

Functions of particular interest include:

- strsplitter: to manupulate and subset vectors of strings

- colourscale: to interpolate a colour scale from an existing colour palette (colours or vector thereof).


### Attribution

This package update was developed and released open-source (GPL-3 License). Please submit Pull Requests or Issues to the GitHub repo if you find any problems.

### Citation

The following information can be retrieved from within an R session by using `citation(tktools)`. Please acknowledge as follows where appropriate:

To cite this package in publications use:

  S. Thomas Kelly (2018). tktools: Package of Personal Utilities. R package version 0.1.0
  https://github.com/TomKellyGenetics/tktools

A BibTeX entry for LaTeX users is

      @Manual{,
        title = {tktools: Package of Personal Utilities},
        author = {S. Thomas Kelly},
        year = {2018},
        note = {R package version 0.1.0},
        url = {https://github.com/TomKellyGenetics/tktools},
      }


