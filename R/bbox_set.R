#' change bounding box in plotdap object
#'
#' \code{bbox_set}changes the bounding box in an plotdap object.
#' Particularly needed if using gganimate::animate()
#' @export
#' @param plotobj valid plotdap object
#' @param landmask optional: if true land is plotted over the data
#' @param xlim optional: new longitude limits of the bounding box, in degrees,
#' otherwise taken from the plotdap object. Longitudes on a (180, 360) scale
#' are converted to (-180, 0) to match how plotdap stores the data.
#' @param ylim optional: new latitude limits of the bounding box, in degrees,
#' otherwise taken from the plotdap object
#' @param interactive optional: if true  creates an interactive version of the graphic
#' @return a ggplot object with reset limits. If interactive is true,  then returns a
#'            ggplot object that will work with the `plotly` package.
#' @details \code{xlim} and \code{ylim} are always given in longitude/latitude,
#' whatever projection the map uses; they are converted to the map's
#' coordinate reference system before being applied.
#'
#' Before plotdap 1.1.1 the signature was \code{bbox_set(plotobj, xlim, ylim)},
#' so older scripts call \code{bbox_set(p, xlim, ylim)} without naming the
#' arguments. Such calls are still accepted: if the second argument is a
#' numeric vector of length 2 it is taken as \code{xlim} and the third as
#' \code{ylim}, with \code{landmask} left at its default.
#'
#' @examples
#' p <- plotdap()
#' p <- add_tabledap(p, sardines, ~subsample_count)
#' xlim = c(-125, -115)
#' ylim <- c(30., 50.)
#' p <- bbox_set(p, xlim = xlim, ylim = ylim)

bbox_set <- function(plotobj, landmask = TRUE, xlim = NULL, ylim = NULL, interactive = FALSE) {
  if (!inherits(plotobj, "ggplotdap")) {
    stop("object passed not a plotdap object")
  }
  # pre-1.1.1 positional call bbox_set(p, xlim, ylim): `landmask` received
  # xlim and `xlim` received ylim
  if (is.numeric(landmask) && length(landmask) == 2) {
    if (!is.null(ylim)) {
      stop("bbox_set(): landmask must be TRUE or FALSE; ",
           "pass limits as xlim = , ylim = ", call. = FALSE)
    }
    ylim <- xlim
    xlim <- landmask
    landmask <- TRUE
  }
  if (!is.logical(landmask) || length(landmask) != 1 || is.na(landmask)) {
    stop("bbox_set(): landmask must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.null(xlim)){
    if (length(xlim) != 2) {
      stop("xlim does not contain two items")
    }
    if (xlim[1] > xlim[2]) {
      stop("xlim improperly ordered")
    }
  }
  if (!is.null(ylim)){
    if (length(ylim) != 2) {
      stop("ylim does not contain two items")
    }
    if (ylim[1] > ylim[2]) {
      stop("ylim improperly ordered")
    }
  }
  gg <- plotobj$ggplot
  # NOTE: add_plotdap_tooltips() edits layer 2 in place
  # (ggplot2 layers are environments), which is why the edit survives the
  # `plotobj$ggplot <- gg` below. Fragile: depends on layer order.
  if (interactive) {plotobj <- add_plotdap_tooltips(plotobj)}
  layer_data <- lapply(gg$layers, function(y) y$layer_data(gg$data))
  if (landmask) {
    gg$layers[1:2] <- rev(gg$layers[1:2])
    layer_data[1:2] <- rev(layer_data[1:2])
    data_lims <- .layers_range(layer_data[1], plotobj$crs)
    plotobj$ggplot <- gg
  } else {
    data_lims <- .layers_range(layer_data[-1], plotobj$crs)
  }

  # The old code validated xlim/ylim and then overwrote them with the data
  # range, so user-supplied limits never took effect. They are now used,
  # interpreted as lon/lat and converted to the plot CRS.
  lims <- .resolve_limits(xlim, ylim, data_lims$xlim, data_lims$ylim,
                          plotobj$crs)

  plotobj$ggplot <- plotobj$ggplot + ggplot2::coord_sf(
                 crs = plotobj$crs, datum = plotobj$datum,
                  xlim = lims$xlim, ylim = lims$ylim
                 )
  # plotly::ggplotly() turns a geom_raster() grid into a heatmap, and plotly
  # always draws heatmaps beneath line traces, so the graticule would be
  # drawn over the data. Drop the grid lines in that case; the axis labels
  # stay. (With the old polygon grids the data hid the graticule anyway.)
  has_raster <- any(vapply(plotobj$ggplot$layers,
                           function(l) inherits(l$geom, "GeomRaster"),
                           logical(1)))
  if (interactive && has_raster) {
    plotobj$ggplot <- plotobj$ggplot +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank())
  }
  plotobj$ggplot
}


# Add tooltip data to plotdap object (modifies layer 2)
add_plotdap_tooltips <- function(plotdap_obj) {

  # Get the data from layer 2
  data_layer <- plotdap_obj$ggplot$layers[[2]]
  plot_data <- data_layer$data

  if (inherits(plot_data, "sf")) {
    # Extract centroid coordinates
    coords <- sf::st_coordinates(sf::st_centroid(plot_data$geometry))
    plot_data$lon <- coords[, "X"]
    plot_data$lat <- coords[, "Y"]
  } else {
    # geom_raster() layer: x/y are cell centres in the plot CRS
    crs <- plotdap_obj$crs
    if (inherits(crs, "crs") && !is.na(crs) && !isTRUE(sf::st_is_longlat(crs))) {
      pts <- sf::st_as_sf(plot_data[c("x", "y")], coords = c("x", "y"), crs = crs)
      coords <- sf::st_coordinates(sf::st_transform(pts, 4326))
      plot_data$lon <- coords[, "X"]
      plot_data$lat <- coords[, "Y"]
    } else {
      plot_data$lon <- plot_data$x
      plot_data$lat <- plot_data$y
    }
  }

  # The plotted variable is whatever the layer maps to fill (griddap) or
  # colour (tabledap). Evaluating that mapping also handles formulas such as
  # ~log2(subsample_count). (Picking the first non-coordinate column, as
  # before, chose `time` for tabledap data and round() then failed.)
  var_aes <- data_layer$mapping$fill %||% data_layer$mapping$colour
  if (!is.null(var_aes)) {
    var_name <- rlang::as_label(var_aes)
    var_values <- rlang::eval_tidy(var_aes, plot_data)
  } else {
    var_name <- setdiff(names(plot_data),
                        c("x", "y", "variable", "geometry", "lon", "lat"))[1]
    var_values <- plot_data[[var_name]]
  }
  if (is.numeric(var_values)) var_values <- round(var_values, 3)

  # Create tooltip text
  plot_data$hover_text <- paste0(
    "Lon: ", round(plot_data$lon, 3), "\n",
    "Lat: ", round(plot_data$lat, 3), "\n",
    var_name, ": ", var_values
  )

  # Update the layer
  plotdap_obj$ggplot$layers[[2]]$data <- plot_data
  plotdap_obj$ggplot$layers[[2]]$mapping$text <- quote(hover_text)

  # Return modified object
  return(plotdap_obj)
}

utils::globalVariables("hover_text")
