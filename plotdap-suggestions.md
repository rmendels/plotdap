# plotdap: Suggested Code Improvements and New Features

Notes from a review session (2026-11-19). The code-improvement suggestions are
based on code snippets reviewed in that session and on the package notes in
`CLAUDE.md`; the source files were not read in full, so each item should be
checked against the actual code before acting on it.

Guiding constraint for all new features: **add new functionality as new
arguments with defaults**, so existing user scripts keep working. This matters
doubly because `rerddapXtracto::plotTrack()` and `plotBBox()` wrap
`add_tabledap()` and `add_griddap()`, so a signature change there is a breaking
change for that package too.

---

## Part 1: Code improvements (sf / ggplot2 with sf)

### 1. Grid layers in the ggplot2 method: stop converting rasters to polygons

**Current:** `add_griddap()` (ggplot2 method) uses `raster::rasterToPolygons()`
then `geom_sf()`.

**Issue:** every pixel becomes a polygon. This is slow, uses a lot of memory,
produces large output, and is likely why `maxpixels` must be kept small.

**Options:**

- `stars::geom_stars()`: plots the grid directly and works with `coord_sf()`.
- `tidyterra::geom_spatraster()`: if/when the package migrates from `raster`
  to `terra`.
- `geom_raster()` on a data frame of projected x/y values with
  `coord_sf(crs = ...)`. Note that `geom_raster()` does not reproject, so
  reproject the grid first.

**Discussion:** this is the largest gain in speed and output quality. It
touches the same code (`add_griddap.R`, `get_raster()`, `is_raster()`) as a
`raster` → `terra` migration, so it is best done as part of that refactor
rather than separately.

### 2. Use `coord_sf(default_crs = ...)` for limits in `bbox_set()`

**Current:**

```r
coord_sf(crs = x$crs, datum = x$datum, xlim = xlim, ylim = ylim)
```

**Issue:** `xlim`/`ylim` are interpreted in the *plot's* projection. If `crs`
is a projected CRS, a user who passes limits in degrees gets the wrong box.

**Suggested (recent ggplot2):**

```r
coord_sf(crs = x$crs, default_crs = sf::st_crs(4326),
         xlim = xlim, ylim = ylim, datum = x$datum)
```

**Discussion:** `default_crs` makes the limits (and any non-sf layers added
later) be read as lon/lat, which is almost certainly what users mean. This is
the smallest change with real user benefit. To preserve backward
compatibility, consider exposing it through a new argument with a default.

### 3. Consider `sf::st_shift_longitude()` for 0–360 data

**Current:** `latlon_adjust()` handles the 0–360 vs −180–180 ambiguity by hand.

**Suggestion:** sf's `st_shift_longitude()` moves geometries into 0–360. Using
it for point layers and the land mask could replace some custom code.

**Discussion:** worth investigating, but keep the existing ambiguity-warning
logic in `latlon_is_valid()` / `latlon_adjust()`.

### 4. Land-mask geometry validity

**Issue:** if the land mask is built from `maps` / world polygons and cropped
to the bounding box, s2 (used by sf by default for lon/lat) can throw errors on
invalid or dateline-crossing polygons.

**Suggestion:** run `sf::st_make_valid()` before `st_crop()` /
`st_intersection()`, or wrap just that step in `sf::sf_use_s2(FALSE)`.

### 5. Tooltips rely on the data layer being at index 2

**Current:** `add_plotdap_tooltips()` (in `bbox_set.R`) assumes the data layer
is layer 2 of `$ggplot$layers`.

**Issue:** breaks as soon as layer order changes (e.g. extra `add_ggplot()` or
contour layers).

**Suggestion:** find the layer by class or by a marker added when the layer is
created, or build the data layer with an interactive geom from the start
(e.g. `ggiraph::geom_sf_interactive()`), if that matches the current approach
to interactivity.

### 6. Clarify the bounding-box fallback in `print.plotdap()`

**Current:**

```r
c(xlim[1], ylim[1], xlim[2], ylim[2]) %||% get_bbox(x$layers[[1]])
```

**Discussion:** this does work. When there are no data layers, `xlim` is
`NULL`, so the `c(...)` is `NULL` and the fallback is used. But it is easy to
misread as dead code. An explicit `if (is.null(xlim)) ... else ...` would make
the intent clear. This is optional.

---

## Part 2: New feature ideas

Grouped by estimated value. Some may already exist in partial form; check the
source first.

### High value, natural fit for ERDDAP data

1. **Vector fields: `add_griddap_vectors()`**
   Currents and winds come as u/v pairs, and the package already bundles
   `QMwind`. Draw arrows on a thinned grid: `geom_segment()` or
   `metR::geom_vector()` for ggplot2, `arrows()` for base graphics. Probably
   the most-requested map type that plotdap can't make yet.

2. **Tracks and trajectories**
   Gliders, drifters, Argo floats, ship tracks and animal tags are tabledap
   data that should be drawn as lines grouped by an ID column, not as
   unconnected points. For example `add_tabledap(..., group = ~platform_id)`
   (default `NULL` = current behaviour) builds `LINESTRING`s, with color able
   to vary along the track.

3. **Bathymetry / topography background: `add_bathymetry()`**
   Fetch ETOPO or SRTM from ERDDAP for the current bounding box and draw it as
   shaded relief or depth contours. Fits the package well because the data
   comes from ERDDAP itself.

4. **Anomaly and difference maps**
   Plot `grid_a - grid_b`, or a grid minus a climatology, with a diverging
   palette centered on zero (e.g. `cmocean::balance`). Users currently have to
   work with the raw arrays to do this.

### Medium value: usability and publication output

5. **Faceting by time as an alternative to animation**
   Static small multiples (`facet_wrap(~time)`) for papers and reports, where
   a GIF can't be used.

6. **Color-scale controls**
   Settable limits, log transforms (useful for chlorophyll), and above all
   fixed limits shared by every animation frame, so colors mean the same thing
   from frame to frame.

7. **Map furniture**
   Scale bar and north arrow (`ggspatial`), an optional inset locator map, and
   higher-resolution coastlines, borders and lakes via `rnaturalearth`.

8. **Automatic caption / attribution**
   Pull the dataset title, institution, time range and an ERDDAP citation from
   the metadata `rerddap` returns, e.g. an `attribution = TRUE` option
   (default `FALSE`).

9. **Discrete variables in `add_tabledap()`**
   Species, QC flags or platform types with categorical color scales, and
   point size mapped to a variable.

### Bigger additions: worth considering, but more work

10. **Clip to a polygon**
    Mask a grid to an MPA, an EEZ or a user-supplied `sf` polygon. Pairs
    naturally with `rerddapXtracto::rxtractogon()`.

11. **Projection presets**
    Ready-made `crs` options such as `"pacific"` (centered on 180°),
    `"arctic"` and `"antarctic"` that handle the dateline and poles properly.
    A common source of user frustration.

12. **Interactive web-map output**
    A `leaflet` or `mapview` backend alongside base and ggplot2.

13. **Inputs other than rerddap objects**
    Accept plain `sf`, `stars` / `terra` or NetCDF files, so users can combine
    ERDDAP data with their own layers.

### Suggested starting points

Items **1 (vectors), 2 (tracks) and 6 (color-scale controls)**: cheap to build
relative to how much users would use them, and they build on existing
functions.
