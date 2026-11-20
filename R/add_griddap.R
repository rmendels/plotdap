#' Add rerddap::griddap() data to a plotdap map
#'
#' \code{add_griddap} adds the data from an 'rerddap::griddap() call to
#' a 'plotdap' map
#' @param plot a \link{plotdap} object.
#' @param grid a \link[rerddap]{griddap} object.
#' @param var a formula defining a variable, or function of variables to visualize.
#' @param fill either a character string of length 1 matching a name in the
#' package \code{cmocean} or a vector of color codes.
#' This defines the colorscale used to encode values
#' of \code{var}.
#' @param maxpixels integer > 0. Maximum number of cells to use for the plot.
#' If maxpixels < ncell(x), the grid is resampled before plotting.
#' The default is small so the default plot is fast; larger values
#' give finer resolution.
#' @param time how to resolve multiple time frames. Choose one of the following:
#' \itemize{
#'   \item A function to apply to each observation at a particular location
#'   (\link{mean} is the default).
#'   \item A character string (of length 1) matching a time value.
#' }
#' @param animate whether to animate over the \code{time} variable (if it exists).
#' Currently only implemented for \code{method='ggplot2'} and requires the
#' gganimate package.
#' @param cumulative - if animation should be cumulative -default FALSE
#' @param render how the grid is drawn when \code{method='ggplot2'}
#' (ignored for \code{method='base'}). \code{"raster"} (the default) draws
#' the grid as an image with \code{geom_raster()}, which is much faster and
#' allows a larger \code{maxpixels}. \code{"polygon"} converts every cell to
#' a polygon and draws it with \code{geom_sf()}, as in plotdap 1.2.x and earlier.
#' @param baseline optional. When given, the map shows the anomaly
#' \code{var - baseline} instead of \code{var}. Either a single number, or a
#' \link[rerddap]{griddap} object such as a climatology or an earlier period.
#' A \code{griddap} baseline with several time steps is averaged over time,
#' and is interpolated onto the grid of \code{grid} if the two grids differ.
#' Unless \code{fill} is given, anomalies use the diverging \code{cmocean}
#' palette "balance" with color limits symmetric about zero.
#' @param baseline_var a formula for the variable in \code{baseline}; defaults
#' to \code{var}. Use it when the baseline dataset names the variable
#' differently.
#' @param ... arguments passed along to \code{geom_raster()} when
#' \code{render = "raster"}, or to \code{geom_sf()} when
#' \code{render = "polygon"} (if \code{method='ggplot2'}, otherwise ignored).
#' @return A plotdap object
#' @export
#' @rdname add_griddap
#' @examples
#'
#' # base plotting tends to be faster,
#' # but is less extensible plotdap("base")
#'
#' # actual datasets in data folder to meet execution timings
#'
#'
#'  # murSST <- rerddap::griddap(
#'  #  ' jplMURSST41', latitude = c(35, 40), longitude = c(-125, -120.5),
#'  #   time = c('last', 'last'), fields = 'analysed_sst'
#'  # )
#'
#'  # QMwind <- rerddap::griddap(
#'  #  'erdQMwindmday', time = c('2016-11-16', '2017-01-16'),
#'  #  latitude = c(30, 50), longitude = c(210, 240),
#'  #  fields = 'x_wind'
#'  #  )
#'
#' p <- plotdap(crs = "+proj=robin")
#' p <- add_griddap(p, murSST, ~analysed_sst)
#'
#' # the pre-1.3 polygon rendering
#' # p <- add_griddap(plotdap(), murSST, ~analysed_sst, render = "polygon")
#'
#'  # p <- plotdap(mapTitle = "Average wind over time")
#'  # p <- add_griddap(p, QMwind, ~x_wind)
#'
#' # p <- plotdap("base", crs = "+proj=robin")
#' # p <- add_griddap(p, murSST, ~analysed_sst)
#'
#' # layer tables on top of grids
#' require(magrittr)
#' p <- plotdap("base") %>%
#'   add_griddap(murSST, ~analysed_sst) %>%
#'   add_tabledap(sardines, ~subsample_count)
#'
#' # multiple time periods
#' p <- plotdap("base", mapTitle = "Average wind over time")
#' p <- add_griddap(p, QMwind, ~x_wind)
#'
#' # anomaly: one month minus the mean of all three months
#' p <- plotdap(mapTitle = "December 2016 x_wind anomaly")
#' p <- add_griddap(p, QMwind, ~x_wind, time = "2016-12-16T00:00:00Z",
#'                  baseline = QMwind)
#'
#' # anomaly from a fixed value
#' p <- add_griddap(plotdap(), murSST, ~analysed_sst, baseline = 12)
#'

add_griddap <- function(plot, grid, var, fill = "viridis",
                        maxpixels = 10000, time = mean, animate = FALSE,
                        cumulative = FALSE, render = c("raster", "polygon"),
                        baseline = NULL, baseline_var = NULL,
                        ...) {
  if (!is.grid(grid))
    stop("The `grid` argument must be a `griddap()` object", call. = FALSE)
  if (!lazyeval::is_formula(var))
    stop("The `var` argument must be a formula", call. = FALSE)
  if (!is.function(time) && !is.character(time))
    stop("The `time` argument must be a function or a character string",
         call. = FALSE)
  render <- match.arg(render)
  anomaly <- !is.null(baseline)
  if (anomaly) {
    if (!is.grid(baseline) &&
        !(is.numeric(baseline) && length(baseline) == 1 && is.finite(baseline))) {
      stop("The `baseline` argument must be a `griddap()` object or a single number",
           call. = FALSE)
    }
    if (!is.null(baseline_var) && !lazyeval::is_formula(baseline_var)) {
      stop("The `baseline_var` argument must be a formula", call. = FALSE)
    }
    # diverging palette by default for anomalies
    if (missing(fill)) fill <- "balance"
  }

  # create raster object from filename;
  # otherwise create a sensible raster from data
  r <- get_raster(grid, var)

  # checks for naming and numeric lat/lon
  latlon_is_valid(r)
  # adjust to ensure everthing is on standard lat/lon scale
  r <- latlon_adjust(r)

  # if necessary, reduce a RasterBrick to a RasterLayer
  # http://gis.stackexchange.com/questions/82390/summarize-values-from-a-raster-brick-by-latitude-bands-in-r
  # (is.function() first: body() of a character `time` is an error, which
  # made time = "<a time value>" fail before it was ever used)
  if (is.function(time) && identical(body(time), quote(UseMethod("mean")))) {
    time <- function(x) mean(x, na.rm = TRUE)
  }
  if (raster::nlayers(r) > 1) {
    if (is.function(time)) {
      r <- raster::calc(r, time)
    } else {
      nm <- make.names(time)
      if (!nm %in% names(r)) {
        warning(
          "The `time` argument doesn't match any of time values.\n",
          sprintf(
            "Valid options include: '%s'",
            paste(unique(grid$data$time), collapse = "', '")
          ),
          call. = FALSE
        )
      }
      r <- r[[nm]]
    }

    if (raster::nlayers(r) > 1 && !animate) {
      stop(
        "The `time` argument hasn't reduced the raster down to a single layer.\n",
        "Either set `animate=TRUE` or provide a suitable value to `time`.",
        call. = FALSE
      )
    }
  }

  # anomaly: subtract the baseline (before thinning and projection, so
  # both are on the data's own lon/lat grid)
  if (anomaly) {
    r <- .subtract_baseline(r, baseline, baseline_var %||% var)
  }

  # simplify raster, if necessary
  n <- raster::ncell(r)
  if (n > maxpixels) {
    message("grid object contains more than ", maxpixels, " pixels")
    message("increase `maxpixels` for a finer resolution")
    rnew <- raster::raster(
      nrow = floor(raster::nrow(r) * sqrt(maxpixels / n)),
      ncol = floor(raster::ncol(r) * sqrt(maxpixels / n)),
      crs = raster::crs(r),
      ext = raster::extent(r)
    )
    if (inherits(r, "RasterBrick")) {
      for (i in seq_len(raster::nlayers(r))) {
        junk <- raster::resample(r[[i]], rnew, method = 'bilinear')
        if (i == 1) {
          temp <- junk
        } else {
          temp <- raster::addLayer(temp, junk)
        }
      }
      r <- raster::brick(temp)
    } else {
      r <- raster::resample(r, rnew, method = 'bilinear')
    }
  }

  # assumes we apply sf::st_crs() to plot on initiation
  # Projecting here means the raster cells form a regular grid in the
  # plot's CRS, which is what geom_raster() needs.
  if (inherits(plot$crs, "crs")) {
    crs_string <- plot$crs$proj4string
    if (!is.na(plot$crs$epsg)) {
      epsg_string <- paste0("+init=epsg:", plot$crs$epsg)
      crs_string <- paste(epsg_string, crs_string)
    }
    r <- raster::projectRaster(r, crs = crs_string)
  }

  # color scale
  if (length(fill) == 1) {
     if (fill == 'viridis') {
       cols <- viridis::viridis(256)
     } else {
       cols <- cmocean::cmocean(fill)(256)
     }
  }  else {
     cols <- fill
  }

  # anomalies: color limits symmetric about zero, so zero is the middle
  # color of a diverging palette
  zlim <- NULL
  if (anomaly) {
    max_abs <- suppressWarnings(max(abs(raster::values(r)), na.rm = TRUE))
    if (is.finite(max_abs) && max_abs > 0) zlim <- c(-max_abs, max_abs)
  }

  # ggplot2 method
  if (is_ggplotdap(plot)) {
    var_name <- lazyeval::f_text(var)
    scale_name <- if (anomaly) paste(var_name, "anomaly") else var_name

    grid_layers <- if (identical(render, "raster")) {
      .grid_layer_raster(r, var_name, cols, ..., .limits = zlim,
                         .scale_name = scale_name)
    } else {
      .grid_layer_polygon(r, var_name, cols, ..., .limits = zlim,
                          .scale_name = scale_name)
    }
    # a second grid (e.g. one over add_bathymetry(type = "shade")) needs its
    # own fill scale, as add_tabledap() does for colour
    if (plot$ggplot$scales$has_scale("fill")) {
      grid_layers <- c(list(ggnewscale::new_scale_fill()), grid_layers)
    }

    if (animate) {
      try_gganimate()
      plot$animate <- TRUE
      # one frame per raster layer (time step)
      plot$nper <- raster::nlayers(r)
      plot$ggplot <- plot$ggplot +
        gganimate::transition_manual(variable, cumulative = cumulative) +
        ggplot2::labs(title = "{current_frame}")
    }

    return(do.call(add_ggplot, c(list(plot), grid_layers)))
  }

  # base method
  if (animate) {
    warning(
      "Animations are currently only implemented for `method='ggplot2'`",
      call. = FALSE
    )
  }

  # TODO: more props!
  grid <- structure(
    r, props = list(
      name = if (anomaly) paste(lazyeval::f_text(var), "anomaly") else
        lazyeval::f_text(var),
      values = raster::values(r),
      color = cols,
      zlim = zlim
    )
  )

  # Throw a warning if the grid extent overlaps with another grid?
  plot$layers <- c(
    plot$layers, list(grid)
  )

  plot
}


# ------------------------------------------------------------------------
# Internal helpers for add_griddap() (ggplot2 method)
# ------------------------------------------------------------------------

# New default: one row per cell centre, drawn as an image by geom_raster().
# r is already projected to the plot CRS, so x/y are plot coordinates and
# coord_sf() (with default_crs = NULL) uses them as-is.
.grid_layer_raster <- function(r, var_name, cols, ..., .limits = NULL,
                               .scale_name = var_name) {
  cell_df <- raster::as.data.frame(r, xy = TRUE)
  layer_cols <- setdiff(names(cell_df), c("x", "y"))
  # long format: one row per cell per time step, same columns
  # ("variable" + var_name) as the polygon version so animation is unchanged.
  # var_name is an argument here, so it must be injected with !! --
  # {{var_name}} would capture the caller's expression, not its value.
  cell_df <- tidyr::gather(cell_df, key = "variable", value = !!var_name,
                           tidyr::all_of(layer_cols))
  # NA cells (land, or outside the projection after projectRaster()) are
  # kept: geom_raster() needs the full regular grid of x/y values, and
  # dropping whole NA columns triggers its "uneven intervals" warning.
  # na.rm = TRUE (unless the user passes their own via ...) stops the
  # "Removed rows containing missing values" warning; na.value = NA in the
  # scale draws any remaining NA cells as transparent.
  geom_args <- utils::modifyList(list(na.rm = TRUE), list(...))

  list(
    do.call(ggplot2::geom_raster, c(
      list(
        data = cell_df,
        mapping = ggplot2::aes(x = .data$x, y = .data$y, fill = .data[[var_name]])
      ),
      geom_args
    )),
    scale_fill_gradientn(name = .scale_name, colors = cols, na.value = NA,
                         limits = .limits),
    # geom_raster() maps x/y, so ggplot2 would title the axes "x" and "y";
    # blank them to match geom_sf() maps (the graticule labels show lon/lat)
    ggplot2::labs(x = NULL, y = NULL)
  )
}

# Old behaviour, kept for render = "polygon".
.grid_layer_polygon <- function(r, var_name, cols, ..., .limits = NULL,
                                .scale_name = var_name) {
  s <- sf::st_as_sf(raster::rasterToPolygons(r))
  vars <- setdiff(names(s), "geometry")
  sg <- sf::st_as_sf(tidyr::gather(s, key = "variable", value = !!var_name,
                                   tidyr::all_of(vars)))
  list(
    geom_sf(data = sg,
            mapping = ggplot2::aes(fill = .data[[var_name]], colour = .data[[var_name]]),
            ...),
    scale_fill_gradientn(name = .scale_name, colors = cols, limits = .limits),
    scale_colour_gradientn(colors = cols, limits = .limits, guide = "none")
  )
}

# Subtract a baseline (a single number, or a griddap object averaged over
# time and interpolated onto r's grid) from every layer of r, keeping r's
# layer names (they label the animation frames).
.subtract_baseline <- function(r, baseline, baseline_var) {
  layer_names <- names(r)
  if (is.numeric(baseline)) {
    r <- r - baseline
  } else {
    b <- .grid_component(baseline, baseline_var, mean)
    if (!raster::compareRaster(r, b, extent = TRUE, rowcol = TRUE,
                               crs = FALSE, stopiffalse = FALSE)) {
      b <- raster::resample(b, r, method = "bilinear")
    }
    r <- r - b
  }
  names(r) <- layer_names
  r
}


