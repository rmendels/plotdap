#' change bounding box in plotdap object
#'
#' \code{bbox_set}changes the bounding box in an plotdap object.
#' Particularly needed if using gganimate::animate()
#' @export
#' @param plotobj valid plotdap object
#' @param landmask optional: if true land is plotted over the data
#' @param xlim optional: new x-values of the bounding box otherwise taken from the plotdap object
#' @param ylim optional: new y-values of the bounding box otherwise taken from the plotdap object
#' @param interactive optional: if true  creates an interactive version of the graphic
#' @return a ggplot object with reset limits. If interactive is true,  then returns a
#'            ggplot object that will work with the `plotly` package.
#'
#' @examples
#' p <- plotdap()
#' p <- add_tabledap(p, sardines, ~subsample_count)
#' xlim = c(-125, -115)
#' ylim <- c(30., 50.)
#' p <- bbox_set(p, xlim = xlim, ylim = ylim)

bbox_set <- function(plotobj, landmask = TRUE, xlim = NULL, ylim = NULL, interactive = FALSE) {
  if (!("ggplotdap" %in% class(plotobj))) {
    stop("object passed not a plotdap object")
  }
  if (!is.null(xlim)){
    if (!(length(xlim) == 2)) {
      stop("xlim does not contain two items")
    }
    if (xlim[1] > xlim[2]) {
      stop("xlim improperly ordered")
    }
  }
  if (!is.null(ylim)){
    if (!(length(ylim) == 2)) {
      stop("ylim does not contain two items")
    }
    if (ylim[1] > ylim[2]) {
      stop("ylim improperly ordered")
    }
  }
  gg <- plotobj$ggplot
  if (interactive) {plotobj <- add_plotdap_tooltips(plotobj)}
  if (landmask) {
    gg$layers[1:2] <- rev(gg$layers[1:2])
    layer_data <- lapply(gg$layers, function(y) y$layer_data(gg$data))
    bbs <- lapply(layer_data[1], sf::st_bbox)
    plotobj$ggplot <- gg
  } else {
    layer_data <- lapply(gg$layers, function(y) y$layer_data(gg$data))
    bbs <- lapply(layer_data[-1], sf::st_bbox)
  }
  xlim <- Reduce(range, lapply(bbs, "[", c("xmin", "xmax")))
  ylim <- Reduce(range, lapply(bbs, "[", c("ymin", "ymax")))
  plotobj$ggplot <- plotobj$ggplot + ggplot2::coord_sf(
                 crs = plotobj$crs, datum = plotobj$datum,
                  xlim = xlim, ylim = ylim
                 )
  plotobj$ggplot
}


# Add tooltip data to plotdap object (modifies layer 2)
add_plotdap_tooltips <- function(plotdap_obj) {

  # Get the data from layer 2
  plot_data <- plotdap_obj$ggplot$layers[[2]]$data

  # Extract centroid coordinates
  coords <- sf::st_coordinates(sf::st_centroid(plot_data$geometry))
  plot_data$lon <- coords[, "X"]
  plot_data$lat <- coords[, "Y"]

  # Get the variable name
  var_name <- setdiff(names(plot_data), c("variable", "geometry", "lon", "lat"))[1]

  # Create tooltip text
  plot_data$hover_text <- paste0(
    "Lon: ", round(plot_data$lon, 3), "\n",
    "Lat: ", round(plot_data$lat, 3), "\n",
    var_name, ": ", round(plot_data[[var_name]], 3)
  )

  # Update the layer
  plotdap_obj$ggplot$layers[[2]]$data <- plot_data
  plotdap_obj$ggplot$layers[[2]]$mapping$text <- quote(hover_text)

  # Return modified object
  return(plotdap_obj)
}
