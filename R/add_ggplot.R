#' Add ggplot2 elements to a plotdap object
#'
#' \code{add_ggplot} allows for plotdap ggplot maps to be modified by
#' further ggplot2 settings
#' @param plot a plotdap object.
#' @param ... arguments passed along to \code{geom_sf()}
#' (if \code{method='ggplot2'}, otherwise ignored).
#' @return A plotdap object
#' @export
#' @rdname add_ggplot
#' @examples
#'
#' p <- plotdap(
#'   crs = "+proj=laea +y_0=0 +lon_0=155 +lat_0=-90 +ellps=WGS84 +no_defs")
#' p <- add_ggplot(
#'  p,
#'  ggplot2::theme_bw()
#' )
#'
#'
add_ggplot <- function(plot, ...) {
  if (!is_plotdap(plot)) {
    stop(
      "The first argument to `add_ggplot()` must be a `plotdap()` object",
      call. = FALSE
    )
  }
  dots <- list(...)
  for (i in seq_along(dots)) {
    plot$ggplot <- plot$ggplot + dots[[i]]
  }
  plot
}
