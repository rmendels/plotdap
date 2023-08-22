#' Add rerddap::griddap() data to a plotdap map
#'
#' \code{add_griddap} adds the data from an 'rerddap::griddap() call to
#' a 'plotdap' map
#' @param plot a \link{plotdap} object.
#' @param grid a \link{griddap} object.
#' @param var a formula defining a variable, or function of variables to visualize.
#' @param fill either a character string of length 1 matching a name in the
#' package \code{cmocean} or a vector of color codes.
#' This defines the colorscale used to encode values
#' of \code{var}.
#' @param maxpixels integer > 0. Maximum number of cells to use for the plot.
#' If maxpixels < ncell(x), sampleRegular is used before plotting.
#' If gridded=TRUE maxpixels may be ignored to get a larger sample
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
#' @param ... arguments passed along to \code{geom_sf()}
#' (if \code{method='ggplot2'}, otherwise ignored).
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
                        cumulative = FALSE, ...) {
  if (!is.grid(grid))
    stop("The `grid` argument must be a `griddap()` object", call. = FALSE)
  if (!lazyeval::is_formula(var))
    stop("The `var` argument must be a formula", call. = FALSE)
  if (!is.function(time) && !is.character(time))
    stop("The `time` argument must be a function or a character string",
         call. = FALSE)

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
        #r[[i]] <- raster::resample(r[[i]], rnew, method = 'bilinear')
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
  if (inherits(plot$crs, "crs")) {
    crs_string <- plot$crs$proj4string
    if (!is.na(plot$crs$epsg)) {
      epsg_string <- paste0("+init=epsg:", plot$crs$epsg)
      crs_string <- paste(epsg_string, crs_string)
    }
    #r <- raster::projectRaster(r, crs = plot$crs$proj4string)
    r <- raster::projectRaster(r, crs = crs_string)
  }

  # color scale
  # cols <- if (length(fill) == 1) rerddap::colors[[fill]] else fill
  if (length(fill) == 1) {
     if (fill == 'viridis') {
       cols <- viridis::viridis(256)
     } else {
       cols <- cmocean::cmocean(fill)(256)

     }
  }  else {
     cols <- fill
  }

  if (is_ggplotdap(plot)) {
    # TODO: not the most efficient approach, but it will have to do for now
    # https://twitter.com/hadleywickham/status/841763265344487424
    s <- sf::st_as_sf(raster::rasterToPolygons(r))
    vars <- setdiff(names(s), "geometry")
    var_name <- lazyeval::f_text(var)
    variable_name <- "variable"
    sg <- sf::st_as_sf(tidyr::gather(s, {{variable_name}},{{var_name}}, vars))
    if (animate) {
      try_gganimate()
      plot$animate <- TRUE
      plot$nper <- length(s) - 1
      plot$ggplot <- plot$ggplot +
        gganimate::transition_manual(variable, cumulative = cumulative) +
        ggplot2::labs(title = "{current_frame}")
    }

    return(
      add_ggplot(
        plot,
        geom_sf(data = sg,
              mapping = ggplot2::aes_string(fill = var_name, colour = var_name), ...),
        scale_fill_gradientn(name = var_name, colors = cols),
        scale_colour_gradientn(colors = cols)
        #ggplot2::guides(colour = "none")
      )
    )
  }

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
