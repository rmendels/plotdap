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

add_griddap <- function(plot, grid, var, fill = "viridis",
                        maxpixels = 10000, time = mean, animate = FALSE,
                        cumulative = FALSE, render = c("raster", "polygon"),
                        ...) {
  if (!is.grid(grid))
    stop("The `grid` argument must be a `griddap()` object", call. = FALSE)
  if (!lazyeval::is_formula(var))
    stop("The `var` argument must be a formula", call. = FALSE)
  if (!is.function(time) && !is.character(time))
    stop("The `time` argument must be a function or a character string",
         call. = FALSE)
  render <- match.arg(render)


  # create raster object from filename;
  # otherwise create a sensible raster from data
  r <- get_raster(grid, var)

  # checks for naming and numeric lat/lon
  latlon_is_valid(r)
  # adjust to ensure everthing is on standard lat/lon scale
  r <- latlon_adjust(r)

  # if necessary, reduce a RasterBrick to a RasterLayer
  # http://gis.stackexchange.com/questions/82390/summarize-values-from-a-raster-brick-by-latitude-bands-in-r
  if (body(time) == 'UseMethod("mean")') {
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

  # ggplot2 method
  if (is_ggplotdap(plot)) {
    var_name <- lazyeval::f_text(var)

    grid_layers <- if (identical(render, "raster")) {
      .grid_layer_raster(r, var_name, cols, ...)
    } else {
      .grid_layer_polygon(r, var_name, cols, ...)
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
      name = lazyeval::f_text(var),
      values = raster::values(r),
      color = cols
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
.grid_layer_raster <- function(r, var_name, cols, ...) {
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
    scale_fill_gradientn(name = var_name, colors = cols, na.value = NA),
    # geom_raster() maps x/y, so ggplot2 would title the axes "x" and "y";
    # blank them to match geom_sf() maps (the graticule labels show lon/lat)
    ggplot2::labs(x = NULL, y = NULL)
  )
}

# Old behaviour, kept for render = "polygon".
.grid_layer_polygon <- function(r, var_name, cols, ...) {
  s <- sf::st_as_sf(raster::rasterToPolygons(r))
  vars <- setdiff(names(s), "geometry")
  sg <- sf::st_as_sf(tidyr::gather(s, key = "variable", value = !!var_name,
                                   tidyr::all_of(vars)))
  list(
    geom_sf(data = sg,
            mapping = ggplot2::aes(fill = .data[[var_name]], colour = .data[[var_name]]),
            ...),
    scale_fill_gradientn(name = var_name, colors = cols),
    scale_colour_gradientn(colors = cols)
  )
}


