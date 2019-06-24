#' Add rerddap::tabledap data to a plotdap map
#'
#' \code{add_tabledap} adds the data from an 'rerddap::tabledap()' call to
#' a 'plotdap' map
#' @param plot a \link{plotdap} object.
#' @param table a \link{tabledap} object.
#' @param var a formula defining a variable, or function of variables to visualize.
#' @param color either a character string of length 1 matching a name in \link[rerddap]{colors}
#' or a vector of color codes. This defines the colorscale used to encode values
#' of \code{var}.
#' @param size the size of the symbol.
#' @param shape the shape of the symbol. For valid options, see the 'pch' values
#' section on \link{points}. \code{plot(0:25, 0:25, pch = 0:25)} also gives a
#' quick visual of the majority of possibilities.
#' @param animate whether to animate over the \code{time} variable (if it exists).
#' Currently only implemented for \code{method='ggplot2'} and requires the
#' gganimate package.
#' @param cumulative - if animation should be cumulative -default FALSE
#' @param ... arguments passed along to \code{geom_sf()} (if \code{method='ggplot2'}, otherwise ignored).
#' @return A plotdap object
#' @export
#' @rdname add_tabledap
#' @examples
#'
#' # base plotting tends to be faster,
#' # but is less extensible plotdap("base")
#'
#' # test datasets in data folder to meet execution timings
#' # code given to extract the data
#'
#'\donttest{
#' sardines <- tabledap(
#'  'FRDCPSTrawlLHHaulCatch',
#'  fields = c('latitude',  'longitude', 'time', 'scientific_name', 'subsample_count'),
#'   'time>=2010-01-01', 'time<=2012-01-01',
#'   scientific_name="Sardinops sagax"
#'   )
#'}
#'
#' p <- plotdap()
#' p1 <- add_tabledap(p, sardines, ~subsample_count)
#' p2 <- add_tabledap(p, sardines, ~log2(subsample_count))
#'
#' # using base R plotting
#' p <- plotdap("base")
#' p <- add_tabledap(p, sardines, ~subsample_count)
#'
#' # robinson projection
#' p <- plotdap(crs = "+proj=robin")
#' p <- add_tabledap(p, sardines, ~subsample_count)
#'
#'
add_tabledap <- function(plot, table, var, color = c("#132B43", "#56B1F7"),
                         size = 1.5, shape = 19, animate = FALSE,
                         cumulative = FALSE, ...) {
  if (!is.table(table))
    stop("The `table` argument must be a `tabledap()` object", call. = FALSE)
  if (!lazyeval::is_formula(var))
    stop("The var argument must be a formula", call. = FALSE)

  table <- format_table(table)

  # checks for naming and numeric lat/lon
  latlon_is_valid(table)
  # adjust to ensure everthing is on standard lat/lon scale
  table <- latlon_adjust(table)

  nms <- names(table)
  # convert to sf
  table <- sf::st_as_sf(
    table, crs = sf::st_crs(4326),
    coords = c(grep(lonPattern(), nms), grep(latPattern(), nms))
  )
  # transform to target projection
  if (inherits(plot$crs, "crs")) {
    table <- sf::st_transform(table, plot$crs)
  }

  # color scale
  cols <- if (length(color) == 1) rerddap::colors[[color]] else color

  if (is_ggplotdap(plot)) {

    if (animate && "time" %in% names(table)) {
      try_gganimate()
      plot$animate <- TRUE
      plot$nper <- length(unique(table$time))
      plot$ggplot <- plot$ggplot +
        gganimate::transition_manual(factor(time), cumulative = cumulative) +
        ggplot2::labs(title = "{current_frame}")
    }

    return(
      add_ggplot(
        plot,
        geom_sf(data = table, mapping = aes_(colour = var),
                size = size, pch = shape, ...),
        scale_colour_gradientn(name = lazyeval::f_text(var), colours = cols)
      )
    )
  }


  if (animate) {
    warning(
      "Animations are currently only implemented for `method='ggplot2'`",
      call. = FALSE
    )
  }

  table <- structure(
    table, props = list(
      name = lazyeval::f_text(var),
      values = lazyeval::f_eval(var, table),
      color = cols,
      size = size,
      shape = shape
    )
  )

  plot$layers <- c(plot$layers, list(table))

  plot
}
