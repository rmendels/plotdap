#' Add bathymetry (ocean depth) to a plotdap map
#'
#' \code{add_bathymetry} adds ocean depth, either as depth contour lines or
#' as shading, from a bathymetry/topography dataset on an ERDDAP server
#' (by default ETOPO1, 'etopo180', on the NOAA CoastWatch West Coast node).
#'
#' @param plot a \link{plotdap} object.
#' @param type \code{"contour"} (the default) draws depth contour lines at
#' \code{depths}; \code{"shade"} colors the ocean by depth.
#' @param depths depths (in metres, positive) at which to draw contours.
#' Only used when \code{type = "contour"}.
#' @param xlim,ylim optional longitude and latitude limits (degrees) of the
#' area to fetch. If not given, the area covered by the layers already added
#' to \code{plot} is used, so either add your data layers first or give the
#' limits (needed when shading underneath data that is added afterwards).
#' @param bathy optional \link[rerddap]{griddap} object with topography
#' already downloaded (for example to avoid fetching it again). If given,
#' \code{dataset}, \code{url}, \code{xlim}, \code{ylim} and \code{maxpixels}
#' are not used to fetch data.
#' @param dataset the ERDDAP dataset ID to fetch topography from.
#' @param url the ERDDAP server URL.
#' @param field the name of the elevation variable in \code{dataset}
#' (negative values are below sea level).
#' @param maxpixels approximate maximum number of grid cells to download;
#' the request is thinned (with an ERDDAP stride) to stay near this.
#' @param color the contour line color (\code{type = "contour"}).
#' @param linewidth the contour line width (\code{type = "contour"}).
#' @param fill the color scale for shading (\code{type = "shade"}); either a
#' \code{cmocean} palette name or a vector of colors.
#' @return A plotdap object
#' @details Layers are drawn in the order they are added. For shading under
#' your data, call \code{add_bathymetry(type = "shade", xlim = , ylim = )}
#' first; for contours over your data, call it last.
#'
#' Fetching requires a network connection; for repeated plots of the same
#' area, fetch once with \code{rerddap::griddap()} and pass it as \code{bathy}.
#' @export
#' @rdname add_bathymetry
#' @examples
#'
#'  # depth contours over a grid
#'  # p <- plotdap()
#'  # p <- add_griddap(p, murSST, ~analysed_sst)
#'  # p <- add_bathymetry(p, depths = c(200, 1000, 3000))
#'
#'  # depth shading under fish survey points
#'  # p <- plotdap()
#'  # p <- add_bathymetry(p, type = "shade", xlim = c(-126, -116), ylim = c(30, 42))
#'  # p <- add_tabledap(p, sardines, ~subsample_count)
#'

add_bathymetry <- function(plot, type = c("contour", "shade"),
                           depths = c(200, 1000, 2000, 3000, 4000),
                           xlim = NULL, ylim = NULL, bathy = NULL,
                           dataset = "etopo180",
                           url = "https://coastwatch.pfeg.noaa.gov/erddap/",
                           field = "altitude", maxpixels = 1e5,
                           color = "grey40", linewidth = 0.3,
                           fill = "deep") {
  if (!is_plotdap(plot))
    stop("The `plot` argument must be a `plotdap()` object", call. = FALSE)
  type <- match.arg(type)

  if (is.null(bathy)) {
    lims <- .bathy_limits(plot, xlim, ylim)
    bathy <- .fetch_bathymetry(lims$xlim, lims$ylim, dataset, url, field,
                               maxpixels)
  } else if (!is.grid(bathy)) {
    stop("The `bathy` argument must be a `griddap()` object", call. = FALSE)
  }
  if (!field %in% names(bathy$data)) {
    stop("Column '", field, "' not found in the bathymetry data", call. = FALSE)
  }

  # depth is positive below sea level; land is NA
  bathy <- .with_depth(bathy, field)

  if (identical(type, "shade")) {
    return(add_griddap(plot, bathy, ~depth, fill = fill,
                       maxpixels = max(maxpixels, nrow(bathy$data))))
  }

  # contours
  nms <- names(bathy$data)
  lat_col <- nms[grep(latPattern(), nms)][1]
  lon_col <- nms[grep(lonPattern(), nms)][1]
  contour_data <- as.data.frame(bathy$data)
  contour_data[[lon_col]] <- ifelse(contour_data[[lon_col]] > 180,
                                    contour_data[[lon_col]] - 360,
                                    contour_data[[lon_col]])
  csf <- .make_contour_sf(contour_data, "depth", n_breaks = NULL,
                          breaks = depths, lon_col = lon_col,
                          lat_col = lat_col, crs = 4326L)
  if (is.null(csf)) {
    warning("add_bathymetry: none of the requested depths occur in this area",
            call. = FALSE)
    return(plot)
  }
  if (inherits(plot$crs, "crs")) {
    csf <- sf::st_transform(csf, plot$crs)
  }

  if (is_ggplotdap(plot)) {
    return(add_ggplot(
      plot,
      ggplot2::geom_sf(data = csf, colour = color, linewidth = linewidth,
                       inherit.aes = FALSE)
    ))
  }

  # base method: drawn by print.plotdap()
  csf <- structure(
    csf,
    class = c("plotdap_lines", class(csf)),
    props = list(color = color, linewidth = linewidth)
  )
  plot$layers <- c(plot$layers, list(csf))
  plot
}


# ------------------------------------------------------------------------
# Internal helpers
# ------------------------------------------------------------------------

# Longitude/latitude limits for the fetch: the user's, or the area covered
# by the layers already in the plot (converted back to lon/lat).
.bathy_limits <- function(plot, xlim, ylim) {
  if (!is.null(xlim) && !is.null(ylim)) {
    if (all(dplyr::between(xlim, 180, 360))) xlim <- xlim - 360
    return(list(xlim = sort(xlim), ylim = sort(ylim)))
  }

  if (is_ggplotdap(plot)) {
    gg <- plot$ggplot
    layer_data <- lapply(gg$layers, function(y) y$layer_data(gg$data))
    data_lims <- .layers_range(layer_data[-1], plot$crs)
  } else {
    bbs <- Filter(Negate(is.null), lapply(plot$layers[-1], get_bbox))
    data_lims <- list(
      xlim = range(unlist(lapply(bbs, "[", c("xmin", "xmax")))),
      ylim = range(unlist(lapply(bbs, "[", c("ymin", "ymax"))))
    )
    if (length(bbs) == 0) data_lims <- list(xlim = NULL, ylim = NULL)
  }
  if (is.null(data_lims$xlim) || is.null(data_lims$ylim) ||
      any(!is.finite(c(data_lims$xlim, data_lims$ylim)))) {
    stop("add_bathymetry(): no data layers to take the area from; ",
         "give `xlim` and `ylim` (degrees)", call. = FALSE)
  }

  crs <- plot$crs
  lonlat <- !inherits(crs, "crs") || is.na(crs) || isTRUE(sf::st_is_longlat(crs))
  ll <- if (lonlat) data_lims else
    .transform_limits(data_lims$xlim, data_lims$ylim, from = crs,
                      to = sf::st_crs(4326))
  list(xlim = xlim %||% ll$xlim, ylim = ylim %||% ll$ylim)
}

# Download topography for the area, thinned to about maxpixels cells.
.fetch_bathymetry <- function(xlim, ylim, dataset, url, field, maxpixels) {
  info <- rerddap::info(dataset, url = url)
  dims <- info$alldata
  spacing <- function(dim_name) {
    att <- dims[[dim_name]]
    sp <- suppressWarnings(as.numeric(
      att$value[att$attribute_name == "average_spacing"]))
    if (length(sp) == 0 || is.na(sp)) 1 / 60 else abs(sp[1])
  }
  dlat <- spacing("latitude")
  dlon <- spacing("longitude")
  ncells <- (diff(ylim) / dlat) * (diff(xlim) / dlon)
  stride <- max(1, ceiling(sqrt(ncells / maxpixels)))

  rerddap::griddap(
    info,
    latitude = ylim,
    longitude = xlim,
    stride = stride,
    fields = field
  )
}

# Add a `depth` column: positive below sea level, NA on land.
.with_depth <- function(bathy, field) {
  out <- unclass(bathy)
  elev <- out$data[[field]]
  out$data$depth <- ifelse(elev < 0, -elev, NA_real_)
  class(out) <- class(bathy)
  out
}
