#' change bounding box in plotdap object
#'
#' \code{bbox_set}changes the bounding box in an plotdap object.
#' Particularly needed if using gganimate::animate()
#' @export
#' @param plotobj valid plotdap object
#' @param xlim new x-values of the bounding box
#' @param ylim new y-values of the bounding box
#' @return a plotdap object
#'
#' @examples
#' p <- plotdap()
#' p <- add_tabledap(p, sardines, ~subsample_count)
#' xlim = c(-125, -115)
#' ylim <- c(30., 50.)
#' p <- bbox_set(p, xlim, ylim)

bbox_set <- function(plotobj, xlim, ylim) {
  if (!("ggplotdap" %in% class(plotobj))) {
    stop("object passed not a plotdap object")
  }
  if (!(length(xlim) == 2)) {
    stop("xlim does not contain two items")
  }
  if (xlim[1] > xlim[2]) {
    stop("xlim improperly ordered")
  }
  if (!(length(ylim) == 2)) {
    stop("ylim does not contain two items")
  }
  if (ylim[1] > ylim[2]) {
    stop("ylim improperly ordered")
  }
  suppressMessages(plotobj <- add_ggplot(
    plotobj, ggplot2::coord_sf(
      crs = plotobj$crs, datum = plotobj$datum,
      xlim = xlim, ylim = ylim
    )
  ))
 plotobj
}
