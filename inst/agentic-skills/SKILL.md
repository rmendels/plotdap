---
name: plotdap
description: Help users write correct R code with plotdap to map and animate rerddap tabledap()/griddap() output. Use when users call plotdap(), add_tabledap(), add_griddap(), add_ggplot(), add_griddap_contours(), or bbox_set(), or need help with map projections (crs), color scales (cmocean/viridis), animation over time (gganimate), or layering multiple ERDDAP(TM) datasets on one map.
metadata:
  author: Roy Mendelssohn (@rmendels)
  version: "1.0"
license: MIT
---

plotdap maps and animates data pulled via `rerddap`'s `tabledap()`/`griddap()` functions in a one-line-per-layer pipeline, in either `ggplot2` or base graphics.

## Core Pattern: plotdap() → add_*dap() → (add_ggplot() | print)

`plotdap()` builds the base map and picks the graphics engine; `add_tabledap()`/`add_griddap()` each add one data layer; layers pipe together, and printing (or `add_ggplot()` for further customization) renders the result.

```r
library(plotdap)
library(rerddap)
library(magrittr)

murSST <- griddap('jplMURSST41',
  latitude = c(35, 40), longitude = c(-125, -120.5),
  time = c('last', 'last'), fields = 'analysed_sst')

p <- plotdap() %>%
  add_griddap(murSST, ~analysed_sst)
p   # print renders it
```

**Wrong pattern:** picking `method` per-layer. `method` is set once, in `plotdap()`, and every subsequent `add_*dap()`/`add_ggplot()` call in the chain follows it — there's no per-layer override.

## ggplot2 vs base

Set once, in `plotdap(method = ...)`:

| | `"ggplot2"` (default) | `"base"` |
|---|---|---|
| Speed | slower, esp. high-res grids | faster |
| Extensible via `add_ggplot()` | yes | no (ignored) |
| `animate = TRUE` | supported (needs `gganimate`) | not supported — warns and ignores |
| `bbox_set()` | works (needs a `ggplotdap` object) | errors — "object passed not a plotdap object" |

```r
plotdap("base") %>% add_griddap(murSST, ~analysed_sst)   # faster, static only
```

## add_tabledap(): Point Data

`table` must be a `rerddap::tabledap()` object; `var` is a one-sided formula. Latitude/longitude columns must be named one of `lat`/`lats`/`latitude` and `lon`/`lons`/`longitude` — anything else throws "Couldn't automatically determine latitude/longitude column."

```r
sardines <- tabledap('FRDCPSTrawlLHHaulCatch',
  fields = c('latitude', 'longitude', 'time', 'scientific_name', 'subsample_count'),
  'time>=2010-01-01', 'time<=2012-01-01',
  'scientific_name="Sardinops sagax"')

p <- plotdap() %>%
  add_tabledap(sardines, ~subsample_count, color = 'algae')

# transform on the fly — var is a formula, so expressions work
p2 <- plotdap() %>% add_tabledap(sardines, ~log2(subsample_count))
```

## add_griddap(): Gridded Data

`grid` must be a `rerddap::griddap()` object (nc or csv). If the grid has more than one time step, `time` must reduce it to a single layer — a function applied per-pixel (`mean` by default) or a character string matching one of the grid's time values — **unless** `animate = TRUE`.

```r
QMwind <- griddap('erdQMwindmday',
  time = c('2016-11-16', '2017-01-16'),
  latitude = c(30, 50), longitude = c(210, 240), fields = 'x_wind')

# multiple time steps averaged down to one layer (default: mean)
p <- plotdap(mapTitle = "Average wind over time") %>%
  add_griddap(QMwind, ~x_wind)

# animate instead of reducing — ggplot2 method + gganimate required
p_anim <- plotdap() %>%
  add_griddap(QMwind, ~x_wind, animate = TRUE)
```

**Wrong pattern:** passing a multi-time-step grid with the default `time = mean` and expecting an animation — without `animate = TRUE` it silently collapses to one averaged frame. Multi-timestep grid + `animate = FALSE` (default) + `time` that doesn't fully reduce it → hard `stop()`: `"The 'time' argument hasn't reduced the raster down to a single layer."`

## Layering Multiple Datasets

`add_griddap()`/`add_tabledap()` calls pipe together onto the same map:

```r
p <- plotdap("base") %>%
  add_griddap(murSST, ~analysed_sst) %>%
  add_tabledap(sardines, ~subsample_count)
```

## Color Scales

`color` (tabledap) / `fill` (griddap) take either a single [cmocean](https://matplotlib.org/cmocean/) palette name (e.g. `'algae'`, `'thermal'`, `'balance'`), `'viridis'`, or an explicit vector of hex colors.

```r
add_tabledap(p, sardines, ~subsample_count, color = 'thermal')
add_griddap(p, murSST, ~analysed_sst, fill = c('#132B43', '#56B1F7'))
```

## Map Projections

Pass a proj4 string or EPSG code to `plotdap(crs = ...)`; every layer added afterward is transformed to it.

```r
p <- plotdap(crs = "+proj=robin") %>%
  add_griddap(murSST, ~analysed_sst)

p_polar <- plotdap(
  crs = "+proj=laea +y_0=0 +lon_0=155 +lat_0=-90 +ellps=WGS84 +no_defs")
```

## Customizing with add_ggplot()

Only affects `method = "ggplot2"` plots — appends arbitrary ggplot2 layers/themes/scales to the internal plot.

```r
p <- plotdap() %>%
  add_griddap(murSST, ~analysed_sst) %>%
  add_ggplot(ggplot2::theme_bw())
```

## Contour Lines: add_griddap_contours()

Adds isolines computed from a griddap **data.frame** (not the plotdap object's raster) on top of an existing `add_griddap()` layer. Must run **before** `add_ggplot()`/printing — it appends directly to the plotdap object's internal ggplot, and later `add_ggplot()` calls stack on top of it, not the reverse.

```r
dat <- griddap(info('jplMURSST41'),
  latitude = c(30, 50), longitude = c(-140, -110),
  time = c('2020-06-15', '2020-06-15'), fields = 'analysed_sst')

p <- plotdap() %>% add_griddap(dat, ~analysed_sst)

# default: 10 evenly-spaced levels
p %>% add_griddap_contours(dat$data, "analysed_sst") %>% add_ggplot()

# explicit levels
p %>% add_griddap_contours(dat$data, "analysed_sst",
  breaks = c(10, 12, 14, 16, 18, 20)) %>% add_ggplot()
```

## Resetting the Bounding Box: bbox_set()

Needed after layering/animating to get sane axis limits, or to add a land mask on top of the data. Requires a `ggplotdap` object (i.e. `method = "ggplot2"`); passing a base-graphics plotdap object throws `"object passed not a plotdap object"`.

```r
p <- plotdap() %>% add_tabledap(sardines, ~subsample_count)
p <- bbox_set(p, xlim = c(-125, -115), ylim = c(30, 50))

# interactive tooltips (works with plotly::ggplotly())
p_interactive <- bbox_set(p, interactive = TRUE)
```

## Bundled Example Datasets

`murSST`, `QMwind`, `sardines` — pre-fetched `griddap()`/`tabledap()` results used throughout the package's own examples so they run offline/quickly.

## Common Mistakes

| Mistake | Fix |
|---|---|
| Naming lat/lon columns something other than `lat`/`lats`/`latitude` or `lon`/`lons`/`longitude` | `add_tabledap()` can't find them and warns; rename the columns first |
| Expecting `animate = TRUE` to work with `method = "base"` | Animation requires `method = "ggplot2"` (and the `gganimate` package) — base silently warns and ignores it |
| Multi-time-step grid, `animate = FALSE`, `time` that doesn't reduce it to one layer | `stop()`: set `animate = TRUE`, or give `time` a reducing function/matching time string |
| Calling `bbox_set()` on a `method = "base"` plot | `bbox_set()` only works on `ggplotdap` (ggplot2-method) objects |
| Calling `add_ggplot()` before `add_griddap_contours()` | Contours must be added first — they append to the internal ggplot directly, so later `add_ggplot()` calls layer on top of them, not vice versa |
| Setting `crs` per-layer | `crs` is set once in `plotdap()`; all layers added afterward inherit it |
| Ambiguous longitude range (all values between 0 and 180) | plotdap can't tell if it's a 0-360 or -180-180 dataset in that range and warns, defaulting to -180/180 — pass unambiguous longitudes if the dataset is actually 0-360 |

## Quick Reference

| Task | Function |
|---|---|
| Start a map, pick engine/projection | `plotdap(method = "ggplot2"/"base", crs = ...)` |
| Add point/track data | `add_tabledap(plot, table, var, ...)` |
| Add gridded data | `add_griddap(plot, grid, var, ...)` |
| Add contour lines to a grid layer | `add_griddap_contours(plot, data, var, ...)` |
| Customize with raw ggplot2 | `add_ggplot(plot, ...)` |
| Reset bounding box / add land mask / interactive tooltips | `bbox_set(plotobj, xlim, ylim, landmask, interactive)` |
| Animate over time | `add_tabledap()`/`add_griddap(..., animate = TRUE)` — ggplot2 method only |
