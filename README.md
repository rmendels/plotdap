
<!-- README.md is generated from README.Rmd. Please edit that file -->

# plotdap

The `plotdap` package makes it easy to map data acquired via the
`rerddap` functions `tabledap()` or `griddap()`.

## Installation

You can install the released version of plotdap from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("plotdap")
```

The development version is available from Github and can be installed
with:

``` r
devtools::install_github('ropensci/plotdap')
```

## Functions

**plotdap** has four main functions and can produce plots in both base
graphics and `ggplot2` graphics, though certain features ony work wih
`ggplot2`:

  - `plotdap()` which sets up the base map:
    
      - `plotdap(method = c("ggplot2", "base"), mapData =
        maps::map("world", plot = FALSE, fill = TRUE), crs = NULL, datum
        = sf::st_crs(4326), mapTitle = NULL, mapFill = "gray80",
        mapColor = "gray90", ...)`

  - `add_tabledap()` which adds the results from a `rerddap::tabledap()`
    call:
    
      - `add_tabledap(plot, table, var, color = c("#132B43", "#56B1F7"),
        size = 1.5, shape = 19, animate = FALSE, cumulative = FALSE,
        ...)`

  - `add_griddap()` which adds the results from a `rerddap::griddap()`
    call:
    
      - `add_griddap(plot, grid, var, fill = "viridis", maxpixels
        = 10000, time = mean, animate = FALSE, cumulative = FALSE, ...)`

  - `add_ggplot()` allows customization of the plot when `ggplot2` is
    used:
    
      - `add_ggplot(plot, ...)`

## Examples

Extensive examples are gven in the vignette, which can also be viewed
at:

<https://rmendels.github.io/Using_plotdap.html>
