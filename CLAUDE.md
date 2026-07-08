# CLAUDE.md

Guidance for Claude Code working **on** the plotdap package source (drop this at the repo root of `rmendels/plotdap`; it currently lives beside `SKILL.md` because no local checkout exists in this environment). plotdap visualizes/animates `rerddap::tabledap()`/`griddap()` output in either base or ggplot2 graphics. R >= 4.3.

## Skill vs. this file

`SKILL.md` in this same directory is LLM-targeted guidance for *writing user code that calls plotdap*. When helping a user *use* plotdap, defer to the skill. **This file is for working on the package source itself.**

## Commands

```r
devtools::document()     # roxygen2 -> man/, NAMESPACE
devtools::load_all()     # load for interactive testing
```

```bash
R CMD build .
R CMD check --as-cran --no-manual plotdap_*.tar.gz
```

`DESCRIPTION` lists `testthat` in `Suggests`, but **there is no `tests/testthat` directory in this repo** — that Suggests entry is currently unused/dead. Don't go looking for a test runner; there isn't one. `devtools::run_examples(run = TRUE)` exercising the roxygen `@examples` (many wrapped in comments/`\donttest{}` since they need a live ERDDAP fetch) plus manual rendering is the actual verification path. CI is minimal: only `rhub.yaml` exists under `.github/workflows` — no `R-CMD-check` workflow runs on every push/PR here, unlike `rerddap`.

## Codebase Shape (`R/`)

- `plotdap.R` — `plotdap()` (entry point, picks `method` once for the whole chain), plus the two `print.ggplotdap()`/`print.plotdap()` S3 methods that do the actual rendering. Also home to most private helpers: `is_plotdap()`, `is_ggplotdap()`, `is_raster()`, `get_bbox()`, `get_raster()` (the griddap-array → `raster` object converter), `latlon_is_valid()`/`latlon_adjust()` (the lat/lon-column-name-detection and 0-360-vs--180-180 ambiguity-warning logic).
- `add_tabledap.R` — point-data layer; converts the `tabledap()` data.frame to `sf` via `st_as_sf()`, builds the color scale, and — for the ggplot2 method — appends a `geom_sf()` layer (with `ggnewscale::new_scale_colour()` inserted for layers after the first, so multiple tabledap layers don't share one color scale).
- `add_griddap.R` — grid-data layer; converts to a `raster::RasterLayer`/`RasterBrick` via `get_raster()` (in `plotdap.R`), reduces multi-time grids via the `time` function/string argument, resamples down to `maxpixels`, then either builds a base-graphics raster layer or (ggplot2 method) rasterToPolygons + `geom_sf()`.
- `add_griddap_contours.R` — isoline layer via `isoband::isolines()`; appends **directly to `p$ggplot`**, so it must run before any subsequent `add_ggplot()` call that should layer visually on top of the contours (order of calls = z-order).
- `add_ggplot.R` — thin `+`-loop appending arbitrary ggplot2 objects to `plot$ggplot`; only meaningful for the ggplot2 method.
- `bbox_set.R` — bounding-box reset / land-mask / interactive-tooltip prep; requires class `ggplotdap`, hard `stop()`s otherwise. `add_plotdap_tooltips()` (private, same file) assumes layer 2 of `$ggplot$layers` is the data layer — will break if the layer ordering assumptions elsewhere in the package change.
- `utils.R` — `is.grid()`/`is.table()` (class checks against `rerddap` output classes), `latValues()`/`lonValues()`/`latPattern()`/`lonPattern()` (the `lat`/`lats`/`latitude` naming-convention detector used by `add_tabledap.R`), `format_table()`, `try_require()`/`try_gganimate()`.
- `imports.R`, `plotdap-package.R` — package-level roxygen (`@import`/`@importFrom` directives, package doc block); no logic.
- `QMwind.R`, `murSST.R`, `sardines.R` — `data/*.rda` documentation stubs for the bundled example griddap/tabledap objects used throughout `@examples`.

## Known Tech Debt: Mixed Tidy-Eval Styles

The package mixes two generations of R's non-standard-evaluation tooling in the same functions: `lazyeval::is_formula()`/`lazyeval::f_eval()`/`lazyeval::f_text()` alongside `rlang::f_rhs()` and `ggplot2::aes(colour = !!rlang::f_rhs(var))`. `add_tabledap.R` and `add_griddap.R` both do this. When touching the `var`-formula handling in either function, keep using whichever style that specific line already uses rather than introducing a third pattern — a full migration off `lazyeval` would be a deliberate, separate refactor, not something to do incidentally.

## Known Dependency Risk: `raster`

`add_griddap.R` depends heavily on the `raster` package (`RasterLayer`/`RasterBrick`, `raster::calc()`, `raster::resample()`, `raster::projectRaster()`). Upstream, `raster` is in long-term maintenance mode in favor of `terra`. A future migration to `terra` would touch most of `add_griddap.R` and `get_raster()`/`is_raster()` in `plotdap.R` — worth knowing before making unrelated changes to those functions, since a rewrite could land at any time.

## Packaging Notes

- `Config/roxygen2/version: 8.0.0` in `DESCRIPTION`, but no `Roxygen: list(markdown = TRUE)` line — check existing comment style (Rd-style, not markdown) before adding new roxygen blocks.
- No formatter/linter config — match surrounding style by hand.
- `plotTrack()`/`plotBBox()` in the sibling `rerddapXtracto` package wrap `add_tabledap()`/`add_griddap()` here — changing either function's signature is a breaking change for that package too.
