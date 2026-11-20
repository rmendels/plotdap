#' Add rerddap::tabledap data to a plotdap map
#'
#' \code{add_tabledap} adds the data from an 'rerddap::tabledap()' call to
#' a 'plotdap' map
#' @param plot a \link[plotdap]{plotdap} object.
#' @param table a \link[rerddap]{tabledap} object.
#' @param var a formula defining a variable, or function of variables to visualize.
#' @param color either a character string of length 1 matching a name in \code{cmocean}
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
#' @param group optional formula naming a column that identifies separate
#' tracks, e.g. \code{~platform_id} for gliders, drifters, floats, ships or
#' tagged animals. When given, each track is drawn as a line (ordered by
#' \code{time} if that column exists) whose color varies along the track
#' with \code{var}, instead of as unconnected points. \code{size} then sets
#' the line width. Default \code{NULL} draws points, as before. For an
#' animation of tracks use \code{cumulative = TRUE} so the tracks grow over time.
#' @param ... arguments passed along to \code{geom_sf()}, or to
#' \code{geom_path()} when \code{group} is given
#' (if \code{method='ggplot2'}, otherwise ignored).
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
#' sardines <- rerddap::tabledap(
#'  'FRDCPSTrawlLHHaulCatch',
#'  fields = c('latitude',  'longitude', 'time', 'scientific_name', 'subsample_count'),
#'   'time>=2010-01-01', 'time<=2012-01-01',
#'   'scientific_name="Sardinops sagax"'
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
#' # tracks: one line per platform, colored by temperature
#' # gliders <- rerddap::tabledap(...)  # data with a platform ID column
#' # p <- plotdap()
#' # p <- add_tabledap(p, gliders, ~temperature, group = ~trajectory)
#'
#'
add_tabledap <- function(plot, table, var, color = c("#132B43", "#56B1F7"),
                         size = 1.5, shape = 19, animate = FALSE,
                         cumulative = FALSE, group = NULL, ...) {
  if (!is.table(table))
    stop("The `table` argument must be a `tabledap()` object", call. = FALSE)
  if (!lazyeval::is_formula(var))
    stop("The var argument must be a formula", call. = FALSE)
  if (!is.null(group) && !lazyeval::is_formula(group))
    stop("The `group` argument must be a formula, e.g. ~platform_id",
         call. = FALSE)

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

  # tracks: sort rows by track, then time, so each track's points are in
  # the order they are joined up
  group_values <- NULL
  if (!is.null(group)) {
    group_values <- lazyeval::f_eval(group, table)
    if (length(group_values) != nrow(table)) {
      stop("The `group` formula must give one value per row of the table",
           call. = FALSE)
    }
    ord <- if ("time" %in% names(table)) {
      order(group_values, table$time)
    } else {
      order(group_values)
    }
    table <- table[ord, ]
    group_values <- group_values[ord]
  }

  # color scale
  # cols <- if (length(color) == 1) rerddap::colors[[color]] else color

  if (length(color) == 1) {
    if (color == 'viridis') {
      cols = viridis::viridis(256)
    } else{
      cols <- cmocean::cmocean(color)(256)
    }
  }  else {
    cols <- color
  }


    if (is_ggplotdap(plot)) {

      if (animate && "time" %in% names(table)) {
        try_gganimate()
        plot$animate <- TRUE
        plot$nper <- length(unique(table$time))
        plot$ggplot <- plot$ggplot +
          gganimate::transition_manual(factor(time), cumulative = cumulative) +
          ggplot2::labs(title = "{current_frame}")
      }
      if (animate && !is.null(group) && !cumulative) {
        warning(
          "Animating tracks without `cumulative = TRUE` shows only one ",
          "point per track in each frame, so no lines are drawn",
          call. = FALSE
        )
      }

      if (is.null(group)) {
        data_layer <- geom_sf(
          data = table, mapping = ggplot2::aes(colour = !!rlang::f_rhs(var)),
          size = size, pch = shape, ...)
      } else {
        # geom_path() lets the colour vary along each track; the
        # coordinates are already in the plot CRS
        track_df <- sf::st_drop_geometry(table)
        xy <- sf::st_coordinates(table)
        track_df$x <- xy[, 1]
        track_df$y <- xy[, 2]
        track_df$.track <- group_values
        data_layer <- ggplot2::geom_path(
          data = track_df,
          mapping = ggplot2::aes(x = .data$x, y = .data$y, group = .data$.track,
                                 colour = !!rlang::f_rhs(var)),
          linewidth = size / 2, ...)
      }
      color_scale <- scale_colour_gradientn(name = lazyeval::f_text(var),
                                            colours = cols)

      # geom_path() maps x/y, which would title the axes "x" and "y";
      # blank them to match the geom_sf() maps
      extras <- if (is.null(group)) list() else list(ggplot2::labs(x = NULL, y = NULL))

      if (length(plot$ggplot$layers) == 1) {
        return(do.call(add_ggplot, c(list(plot, data_layer, color_scale), extras)))
      } else {
        return(do.call(add_ggplot, c(list(plot, ggnewscale::new_scale_colour(),
                                          data_layer, color_scale), extras)))
      }
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
      shape = shape,
      group = group_values
    )
  )

  plot$layers <- c(plot$layers, list(table))

  plot
}
