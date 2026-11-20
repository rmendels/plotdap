#' Add vector arrows (e.g. winds or currents) from rerddap::griddap() data
#'
#' \code{add_griddap_vectors} draws arrows for a vector field, such as wind
#' or ocean currents, whose eastward (u) and northward (v) components are in
#' an 'rerddap::griddap()' result. It is usually added on top of a grid
#' layer from \code{add_griddap()}, but can also be used on its own.
#'
#' @param plot a \link{plotdap} object.
#' @param grid a \link[rerddap]{griddap} object containing both the u and v
#' components, e.g. from a \code{griddap()} call with
#' \code{fields = c('x_wind', 'y_wind')}.
#' @param u a formula giving the eastward component, e.g. \code{~x_wind}.
#' @param v a formula giving the northward component, e.g. \code{~y_wind}.
#' @param time how to resolve multiple time frames. Either a function applied
#' to each component at each location (\link{mean} is the default), or a
#' character string (of length 1) matching a time value.
#' @param maxarrows integer > 0. Approximate maximum number of arrows. The
#' grid is thinned by averaging blocks of neighbouring cells, so each arrow
#' shows the average vector over its block.
#' @param scale numeric > 0. Multiplies the arrow lengths. With the default
#' of 1 the longest arrow is about 90\% of the spacing between arrows.
#' @param color the arrow color.
#' @param linewidth the arrow line width.
#' @param head_length the length of the arrow heads, in centimetres.
#' @param ... arguments passed along to \code{geom_segment()}
#' (if \code{method='ggplot2'}, otherwise ignored).
#' @return A plotdap object
#' @details The arrows are computed in longitude/latitude and then projected,
#' so they point in the correct geographic direction on any map projection.
#' Arrow length shows relative speed within the plot; it is not tied to a
#' fixed unit.
#' @export
#' @rdname add_griddap_vectors
#' @examples
#'
#'  # wind <- rerddap::griddap(
#'  #  'erdQMwindmday', time = c('2016-11-16', '2017-01-16'),
#'  #  latitude = c(30, 50), longitude = c(210, 240),
#'  #  fields = c('x_wind', 'y_wind')
#'  #  )
#'
#'  # speed as the background, arrows on top
#'  # p <- plotdap(crs = "+proj=robin")
#'  # p <- add_griddap(p, wind, ~sqrt(x_wind^2 + y_wind^2), fill = "speed")
#'  # p <- add_griddap_vectors(p, wind, ~x_wind, ~y_wind)
#'
#'  # base graphics, fewer and longer arrows
#'  # p <- plotdap("base")
#'  # p <- add_griddap_vectors(p, wind, ~x_wind, ~y_wind,
#'  #                          maxarrows = 150, scale = 1.5)
#'

add_griddap_vectors <- function(plot, grid, u, v, time = mean,
                                maxarrows = 400, scale = 1,
                                color = "black", linewidth = 0.4,
                                head_length = 0.15, ...) {
  if (!is_plotdap(plot))
    stop("The `plot` argument must be a `plotdap()` object", call. = FALSE)
  if (!is.grid(grid))
    stop("The `grid` argument must be a `griddap()` object", call. = FALSE)
  if (!lazyeval::is_formula(u) || !lazyeval::is_formula(v))
    stop("The `u` and `v` arguments must be formulas", call. = FALSE)
  if (!is.function(time) && !is.character(time))
    stop("The `time` argument must be a function or a character string",
         call. = FALSE)
  if (!is.numeric(maxarrows) || length(maxarrows) != 1 || maxarrows < 1)
    stop("`maxarrows` must be a single number >= 1", call. = FALSE)
  if (!is.numeric(scale) || length(scale) != 1 || scale <= 0)
    stop("`scale` must be a single positive number", call. = FALSE)

  arrows_df <- .vector_arrows(grid, u, v, time, maxarrows, scale, plot$crs)

  if (is_ggplotdap(plot)) {
    return(add_ggplot(
      plot,
      ggplot2::geom_segment(
        data = arrows_df,
        mapping = ggplot2::aes(x = .data$x, y = .data$y,
                               xend = .data$xend, yend = .data$yend),
        colour = color,
        linewidth = linewidth,
        arrow = ggplot2::arrow(length = ggplot2::unit(head_length, "cm")),
        inherit.aes = FALSE,
        ...
      )
    ))
  }

  # base method: stored as a layer and drawn by print.plotdap()
  vec <- structure(
    arrows_df,
    class = c("plotdap_vectors", "data.frame"),
    props = list(
      color = color,
      linewidth = linewidth,
      head_length = head_length
    )
  )
  plot$layers <- c(plot$layers, list(vec))
  plot
}


# ------------------------------------------------------------------------
# Internal helpers
# ------------------------------------------------------------------------

# Build a data frame of arrows (x, y, xend, yend, speed) in the plot CRS.
.vector_arrows <- function(grid, u, v, time, maxarrows, scale, crs) {
  ru <- .grid_component(grid, u, time)
  rv <- .grid_component(grid, v, time)

  # thin by averaging blocks of cells, so each arrow is a block mean.
  # expand = FALSE drops partial blocks at the edges; otherwise their
  # centres fall outside the data and arrows appear beyond the grid.
  fact <- ceiling(sqrt(raster::ncell(ru) / maxarrows))
  if (fact > 1) {
    ru <- raster::aggregate(ru, fact = fact, fun = mean, na.rm = TRUE,
                            expand = FALSE)
    rv <- raster::aggregate(rv, fact = fact, fun = mean, na.rm = TRUE,
                            expand = FALSE)
  }

  du <- raster::as.data.frame(ru, xy = TRUE)
  arrows_df <- data.frame(
    lon = du$x, lat = du$y,
    u = du[[3]], v = raster::values(rv)
  )
  arrows_df <- arrows_df[stats::complete.cases(arrows_df), , drop = FALSE]
  if (nrow(arrows_df) == 0) {
    stop("No non-missing u/v values to draw", call. = FALSE)
  }
  arrows_df$speed <- sqrt(arrows_df$u^2 + arrows_df$v^2)

  # arrow length in degrees of latitude: the fastest arrow spans ~90% of
  # the spacing between arrows (times `scale`)
  spacing <- min(raster::res(ru))
  max_speed <- max(arrows_df$speed)
  deg_per_unit <- if (max_speed > 0) 0.9 * spacing * scale / max_speed else 0

  # east-west degrees shrink with latitude, so stretch u by 1/cos(lat)
  # to keep arrow directions geographically correct. Arrows are centred
  # on their cell: half the length behind it, half in front.
  half_dlon <- 0.5 * arrows_df$u * deg_per_unit / cos(arrows_df$lat * pi / 180)
  half_dlat <- 0.5 * arrows_df$v * deg_per_unit

  start <- .project_xy(arrows_df$lon - half_dlon, arrows_df$lat - half_dlat, crs)
  end <- .project_xy(arrows_df$lon + half_dlon, arrows_df$lat + half_dlat, crs)

  out <- data.frame(
    x = start[, 1], y = start[, 2],
    xend = end[, 1], yend = end[, 2],
    speed = arrows_df$speed
  )
  out[stats::complete.cases(out), , drop = FALSE]
}

# One vector component as a single-layer, lon/lat RasterLayer.
.grid_component <- function(grid, var, time) {
  r <- get_raster(grid, var)
  latlon_is_valid(r)
  r <- latlon_adjust(r)
  if (raster::nlayers(r) > 1) {
    if (is.function(time)) {
      if (identical(time, mean)) {
        time <- function(x) mean(x, na.rm = TRUE)
      }
      r <- raster::calc(r, time)
    } else {
      nm <- make.names(time)
      if (!nm %in% names(r)) {
        stop(
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
    if (raster::nlayers(r) > 1) {
      stop("The `time` argument hasn't reduced the grid to a single time.",
           call. = FALSE)
    }
  }
  r
}

# Project lon/lat coordinates to the plot CRS; returns a 2-column matrix.
.project_xy <- function(lon, lat, crs) {
  if (!inherits(crs, "crs") || is.na(crs) || isTRUE(sf::st_is_longlat(crs))) {
    return(cbind(lon, lat))
  }
  pts <- sf::st_as_sf(data.frame(lon = lon, lat = lat),
                      coords = c("lon", "lat"), crs = 4326)
  sf::st_coordinates(sf::st_transform(pts, crs))[, 1:2, drop = FALSE]
}
