#' Add contour lines to a plotdap object
#'
#' Computes isolines from a griddap data frame and appends them as a
#' \code{geom_sf()} layer to the \strong{plotdap object's internal ggplot}.
#' Call this \emph{before} \code{add_ggplot()} or \code{print()}.
#'
#' @param p         a plotdap object (result of \code{plotdap()} piped through
#'                  \code{add_griddap()}).  \strong{Not} the result of
#'                  \code{add_ggplot()}.
#' @param data      data frame  typically \code{dat$data} from a
#'                  \code{griddap()} call.
#' @param var       character; name of the variable column to contour.
#' @param n_breaks  integer; number of evenly-spaced interior contour levels
#'                  (ignored when \code{breaks} is supplied). Default 10.
#' @param breaks    numeric vector of explicit contour levels, or \code{NULL}.
#' @param lon_col   name of longitude column (default \code{"longitude"}).
#' @param lat_col   name of latitude column  (default \code{"latitude"}).
#' @param color     contour line color (default \code{"black"}).
#' @param linewidth contour line width (default \code{0.3}).
#' @param alpha     opacity 01 (default \code{1}).
#' @param crs       integer EPSG code for the data (default \code{4326}).
#'
#' @return the plotdap object with contour lines added to its internal ggplot.
#'   Pipe the result to \code{add_ggplot()} or \code{print()} as usual.
#' @export
#'
#' @examples
#' \dontrun{
#'   library(rerddap)
#'   library(plotdap)
#'
#'   myURL <- "https://coastwatch.pfeg.noaa.gov/erddap/"
#'   info <- rerddap::info("jplMURSST41", url = myURL)
#'   dat  <- griddap(info,
#'             latitude  = c(30, 50),
#'             longitude = c(-140, -110),
#'             time      = c("2020-06-15", "2020-06-15"),
#'             fields    = "analysed_sst")
#'
#'   p <- plotdap() |>
#'          add_griddap(dat, ~analysed_sst)
#'
#'   ## Default: 10 evenly-spaced contour levels, then render
#'   p |> add_griddap_contours(dat$data, "analysed_sst") |> add_ggplot()
#'
#'   ## Explicit break values
#'   p |> add_griddap_contours(dat$data, "analysed_sst",
#'                              breaks = c(10, 12, 14, 16, 18, 20)) |> add_ggplot()
#'
#'   ## More levels, thinner grey lines
#'   p |> add_griddap_contours(dat$data, "analysed_sst",
#'                              n_breaks = 20, color = "grey40", linewidth = 0.2) |>
#'        add_ggplot()
#' }
add_griddap_contours <- function(p,
                                 data,
                                 var,
                                 n_breaks  = 10L,
                                 breaks    = NULL,
                                 lon_col   = "longitude",
                                 lat_col   = "latitude",
                                 color     = "black",
                                 linewidth = 0.3,
                                 alpha     = 1,
                                 crs       = 4326L) {

  if (!("ggplotdap" %in% class(p))) {
    stop("object passed not a plotdap object")
  }

  csf <- .make_contour_sf(data, var, n_breaks, breaks, lon_col, lat_col, crs)

  if (is.null(csf)) {
    warning("add_griddap_contours: no contours generated  ",
            "check that '", var, "' exists and has sufficient range")
    return(p)
  }

  # p is a plotdap object; add the layer to its internal ggplot
  p$ggplot <- p$ggplot + ggplot2::geom_sf(
    data        = csf,
    color       = color,
    linewidth   = linewidth,
    alpha       = alpha,
    inherit.aes = FALSE
  )

  p
}


#  internal: build sf MULTILINESTRING object from griddap data.frame

.make_contour_sf <- function(data, var, n_breaks, breaks, lon_col, lat_col, crs) {

  stopifnot(is.data.frame(data), var %in% names(data),
            lon_col %in% names(data), lat_col %in% names(data))

  data <- data[!is.na(data[[var]]), , drop = FALSE]
  if (nrow(data) == 0L) return(NULL)

  lons <- sort(unique(data[[lon_col]]))
  lats <- sort(unique(data[[lat_col]]))

  #  determine breakpoints
  z_range <- range(data[[var]], na.rm = TRUE)

  if (is.null(breaks)) {
    # n_breaks interior levels, evenly spaced; exclude the endpoints so the
    # outermost contours sit inside the data range rather than on the edge
    all_breaks <- seq(z_range[1], z_range[2], length.out = n_breaks + 2L)
    breaks <- all_breaks[-c(1L, length(all_breaks))]
  }

  breaks <- sort(unique(breaks))

  # Remove breaks outside the data range
  breaks <- breaks[breaks > z_range[1] & breaks < z_range[2]]
  if (length(breaks) == 0L) return(NULL)

  #  build z matrix (rows = lat ascending, cols = lon)
  z_mat <- matrix(NA_real_, nrow = length(lats), ncol = length(lons))
  z_mat[cbind(match(data[[lat_col]], lats),
              match(data[[lon_col]], lons))] <- data[[var]]

  #  generate isolines
  iso <- isoband::isolines(x = lons, y = lats, z = z_mat, levels = breaks)

  #  convert each level to one sf MULTILINESTRING row
  rows <- lapply(seq_along(iso), function(i) {
    ln <- iso[[i]]
    if (length(ln$x) == 0L) return(NULL)

    ids  <- unique(ln$id)
    mats <- lapply(ids, function(id) {
      mask <- ln$id == id
      cbind(ln$x[mask], ln$y[mask])
    })

    sf::st_sf(
      level    = breaks[i],
      geometry = sf::st_sfc(sf::st_multilinestring(mats), crs = crs),
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(result) || nrow(result) == 0L) return(NULL)
  result
}
