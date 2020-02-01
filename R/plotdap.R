#' Visualize rerddap data
#'
#' Visualize data returned from rerddap servers. Use \code{plotdap()} to initialize
#' a plot, specify the plotting method (specifically, 'base' or 'ggplot2'),
#' and set some global options/parameters. Then use \code{add_tabledap()}
#' and/or \code{add_griddap()} to add "layers" of actual data to be visualized.
#'
#' @details The "ggplot2" method is slower than "base" (especially
#' for high-res grids/rasters), but is more flexible/extensible. Additional ggplot2
#' layers, as well as scale defaults, labels, theming, etc. may be modified via
#' the \code{add_ggplot()} function. See the mapping vignette for an introduction
#' and overview of rerddap's visualization methods --
#' \code{browseVignettes(package = "rerddap")}.
#'
#' @param method the plotting method. Currently ggplot2 and base plotting
#' are supported.
#' @param mapData an object coercable to an sf object via \code{st_as_sf()}.
#' @param crs a coordinate reference system: integer with the epsg code,
#' or character with proj4string.
#' @param datum crs that provides datum to use when generating graticules.
#' Set to \code{NULL} to hide the graticule.
#' @param mapTitle a title for the map.
#' @param mapFill fill used for the map.
#' @param mapColor color used to draw boundaries of the map.
#' @param ... arguments passed along to \code{geom_sf()}
#' (if \code{method='ggplot2'}, otherwise ignored).
#' @return A plotdap object
#' @export
#' @seealso \code{tabledap()}, \code{griddap()}
#' @author Carson Sievert
#' @examples
#'
#' # base plotting tends to be faster (especially for grids),
#' # but is less extensible plotdap("base")
#'
#'  plotdap()
#'  plotdap("base")
#'
#'

plotdap <- function(method = c("ggplot2", "base"),
                    mapData = maps::map('world', plot = FALSE, fill = TRUE),
                    crs = NULL, datum = sf::st_crs(4326),
                    mapTitle = NULL, mapFill = "gray80", mapColor = "gray90",
                    ...) {

  method <- match.arg(method, method)

  # packages used in both methods
  require_packages(c("sf", "maps", "lazyeval", "rgeos", "rgdal", "maptools"))

  # maps is bad at namespacing
  try_require("mapdata", "plotdap")
  bgMap <- sf::st_as_sf(mapData)

  # transform background map to target projection
  if (!is.null(crs)) {
    bgMap <- sf::st_transform(bgMap, crs)
  }

  if (identical(method, "ggplot2")) {
    p <- ggplot() +
      geom_sf(data = bgMap, fill = mapFill, color = mapColor, ...) +
      theme_bw() + ggtitle(mapTitle)
    # keep track of some "global" properties...
    # this seems to be the only way to do things like train axis ranges
    # without a ggplot_build() hack...
    p2 <- list(
      ggplot = p,
      crs = sf::st_crs(bgMap),
      datum = datum
    )
    return(structure(p2, class = c("ggplotdap", class(p))))
  }

  # construct an custom object for base plotting...
  # we might do all the plotting in the print step since we won't know the
  # final x/y limits until then
  dap <- list(
    crs = sf::st_crs(bgMap),
    datum = datum,
    mapTitle = mapTitle,
    mapColor = mapColor,
    mapFill = mapFill,
    layers = list(bgMap)
  )
  structure(dap, class = "plotdap")
}








#' Print a ggplot plotdap object
#'
#' @param x a ggplotdap object
#' @param ... currently unused
#' @export
print.ggplotdap <- function(x, ...) {
    # find a sensible x/y range....assuming all the layer data is sf
  gg <- x$ggplot
  dots <- list(...)
  if (isTRUE(dots$landmask)) {
    gg$layers <- rev(gg$layers)
    layer_data <- lapply(gg$layers, function(y) y$layer_data(gg$data))
    bbs <- lapply(layer_data[1], sf::st_bbox)
    x$ggplot <- gg
  } else {
    layer_data <- lapply(gg$layers, function(y) y$layer_data(gg$data))
    bbs <- lapply(layer_data[-1], sf::st_bbox)
  }
  xlim <- Reduce(range, lapply(bbs, "[", c("xmin", "xmax")))
  ylim <- Reduce(range, lapply(bbs, "[", c("ymin", "ymax")))
  suppressMessages(x <- add_ggplot(
    x, coord_sf(
      crs = x$crs, datum = x$datum,
      xlim = xlim, ylim = ylim
    )
  ))
#  x <- add_ggplot(
#    x, coord_sf(
#      crs = x$crs, datum = x$datum,
#      xlim = xlim, ylim = ylim
#    )
#  )

  if (isTRUE(x$animate)) {
    print(gganimate::animate(x$ggplot, nframes = x$nper,
                             fps = 1, duration = x$nper))
    invisible(x)
  } else {
    print(x$ggplot)
    invisible(x)
  }
}



#' Print a plotdap object
#'
#' @param x a plotdap object
#' @param ... currently unused
#' @export
print.plotdap <- function(x, ...) {

  # remember, unlike ggplotdap, plotdap layers can have both sf and raster objs
  bbs <- lapply(x$layers[-1], get_bbox)
  xlim <- Reduce(range, lapply(bbs, "[", c("xmin", "xmax")))
  ylim <- Reduce(range, lapply(bbs, "[", c("ymin", "ymax")))

  graticule <- sf::st_graticule(
    c(xlim[1], ylim[1], xlim[2], ylim[2]) %||% get_bbox(x$layers[[1]]),
    crs = x$crs,
    datum = x$datum
  )

  # plot the background map
  plot(
    x$layers[[1]],
    xlim = xlim,
    ylim = ylim,
    main = x$mapTitle %||% "",
    col = x$mapFill,
    border = x$mapColor,
    graticule = graticule,
    #setParUsrBB = TRUE,
    bty = "n",
    reset = FALSE,
    ...
  )

  for (i in setdiff(seq_along(x$layers), 1)) {
    layer <- x$layers[[i]]
    props <- attr(layer, "props")
    rng <- range(props$values, na.rm = TRUE)
    pal <- scales::col_numeric(props$color, rng)
    breaks <- stats::quantile(props$values, 0:4/4, na.rm = TRUE)

    # plot rasters first, otherwise we have to fiddle with legends
    if (is_raster(layer)) {
      raster::plot(
        layer,
        add = TRUE,
        legend = FALSE,
        frame.plot = FALSE,
        # TODO: where does this scaling occur? In raster::plot()?
        col = props$color,
        ...
      )
      graphics::legend(
        "bottomright",
        title = props$name,
        legend = format(breaks),
        col = pal(breaks),
        pch = 20
      )
    } else {
      # should be sf POINTS
      plot(
        layer[["geometry"]],
        add = TRUE,
        col = pal(props$values),
        pch = props$shape,
        cex = props$size
      )
      graphics::legend(
        "topright",
        title = props$name,
        legend = format(breaks),
        col = pal(breaks),
        pch = 20
      )
    }
  }
  x
}


# ------------------------------------------------------------------------
# Internal helper functions
# ------------------------------------------------------------------------

is_plotdap <- function(x) {
  inherits(x, c("ggplotdap", "plotdap"))
}

is_ggplotdap <- function(x) {
  is_plotdap(x) && ggplot2::is.ggplot(x$ggplot)
}

is_raster <- function(x) {
  inherits(x, c("RasterLayer", "RasterBrick", "RasterStack"))
}

get_bbox <- function(x) {
  if (inherits(x, "sf")) {
    return(sf::st_bbox(x))
  }
  # TODO: support raster objects, as well?
  if (inherits(x, "RasterLayer")) {
    ext <- as.list(raster::extent(x))
    return(stats::setNames(ext, c("xmin", "xmax", "ymin", "ymax")))
  }
  invisible()
}

get_raster <- function(grid, var) {
  times <- grid$summary$dim$time$vals
  lats <- grid$summary$dim$latitude$vals
  lons <- grid$summary$dim$longitude$vals
  ylim <- range(lats, na.rm = TRUE)
  xlim <- range(lons, na.rm = TRUE)
  ext <- raster::extent(xlim[1], xlim[2], ylim[1], ylim[2])
  r <- if (length(times) > 1) {
    # ensure values appear in the right order...
    # TODO: how to detect a south -> north ordering?
    if ("lat" %in% names(grid$data)) {
      d <- dplyr::arrange(grid$data, time, desc(lat), lon)
    } else {
      d <- dplyr::arrange(grid$data, time, desc(latitude), longitude)
    }
    b <- raster::brick(
      nl = length(times),
      nrows = length(lats),
      ncols = length(lons)
    )
    raster::values(b) <- lazyeval::f_eval(var, d)
    raster::setExtent(b, ext)
  } else {
    if ("lat" %in% names(grid$data)) {
      d <- dplyr::arrange(grid$data, desc(lat), lon)
    } else {
      d <- dplyr::arrange(grid$data, desc(latitude), longitude)
    }
    raster::raster(
      nrows = length(lats),
      ncols = length(lons),
      ext = ext,
      vals = lazyeval::f_eval(var, d)
    )
  }
  names(r) <- make.names(unique(grid$data$time) %||% "")
  r
}


utils::globalVariables(c("time", "desc", "lat", "lon"))

latlon_is_valid <- function(x) {
  if (is_raster(x)) {
    if (!raster::isLonLat(x)) {
      stop(
        "raster object must have a longitude/latitude coordinate reference system (CRS)",
        call. = FALSE
      )
    }
    return(TRUE)
  }

  if (!is.numeric(latValues(x))) {
    stop("Latitudes must be numeric", call. = FALSE)
  }

  if (!is.numeric(lonValues(x))) {
    stop("Longitudes must be numeric", call. = FALSE)
  }

  invisible(TRUE)
}

latlon_adjust <- function(x) {
  if (is.table(x)) {

    lonIDX <- grep(lonPattern(), names(x))
    lon <- range(x[[lonIDX]], na.rm = TRUE)

    if (all(dplyr::between(lon, 0, 180))) {

      warning(
        "Can't determine whether longitude is on (0, 360) or (-180, 180) scale\n",
        "Defaulting to (-180, 180) scale...",
        call. = FALSE
      )

    } else if (all(dplyr::between(lon, 180, 360))) {

      #idx <- isTRUE(lon > 180)
      x[[lonIDX]] <- lon - 360

    } else if (all(dplyr::between(lon, -180, 180))) {

      # nothing to do....

    } else {

      # TODO: report the invalid values?
      warning("Invalid longitude values", call. = TRUE)

    }

    return(x)
  }

  if (is_raster(x)) {

    ext <- raster::extent(x)
    lon <- c(ext@xmin, ext@xmax)

    if (all(dplyr::between(lon, 0, 180))) {

      warning(
        "Can't determine whether longitude is on (0, 360) or (-180, 180) scale\n",
        "Defaulting to (-180, 180) scale...",
        call. = FALSE
      )

    } else if (all(dplyr::between(lon, 180, 360))) {

      newExt <- raster::extent(
        c(c(ext@xmin, ext@xmax) - 360, ext@ymin, ext@ymax)
      )
      x <- raster::setExtent(x, newExt, keepres = TRUE)

    } else if (all(dplyr::between(lon, -180, 180))) {

      # nothing to do....

    } else if (all(dplyr::between(lon, 0, 360))) {

      # nothing to do....
    } else {

      # TODO: report the invalid values?
      warning("Invalid longitude values", call. = TRUE)

    }

    return(x)
  }

  # throw an error?
  x
}


latValues <- function(x) {
  latIDX <- grep(latPattern(), names(x))
  if (length(latIDX) != 1) {
    stop(
      "Couldn't find latitude variable. Must be named one of the following:\n",
      "'lat', 'lats', 'latitude'",
      call. = FALSE
    )
  }
  x[[latIDX]]
}

lonValues <- function(x) {
  lonIDX <- grep(lonPattern(), names(x))
  if (length(lonIDX) != 1) {
    stop(
      "Couldn't find longitude variable. Must be named one of the following:\n",
      "'lon', 'lons', 'longitude'",
      call. = FALSE
    )
  }
  if (!is.numeric(x[[lonIDX]])) {
    stop("Longitude must be numeric", call. = FALSE)
  }
  x[[lonIDX]]
}

latPattern <- function() "^lat$|^lats$|^latitude$"
lonPattern <- function() "^lon$|^lons$|^longitude$"

# valid names for latitude
latNames <- function() {
  sub("\\$", "", sub("\\^", "", strsplit(latPattern(), "|", fixed = TRUE)[[1]]))
}

lonNames <- function() {
  sub("\\$", "", sub("\\^", "", strsplit(lonPattern(), "|", fixed = TRUE)[[1]]))
}


utils::globalVariables("variable")
