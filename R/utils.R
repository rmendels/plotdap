
"%||%" <- function(x, y) {
  if (length(x) > 0) x else y
}

is.grid <- function(x) {
  inherits(x, c("griddap_nc", "griddap_csv"))
}

is.table <- function(x) {
  inherits(x, "tabledap")
}

require_packages <- function(x) {
  for (i in x) {
    if (system.file(package = i) == "") {
      stop(sprintf("Please install package: '%s'", i), call. = FALSE)
    }
  }
  invisible(TRUE)
}

# copied from ggplot2:::try_require...thanks Hadley
try_require <- function(package, fun) {
  if (requireNamespace(package, quietly = TRUE)) {
    library(package, character.only = TRUE)
    return(invisible())
  }
  stop("Package `", package, "` required for `", fun, "`.\n",
       "Please install and try again.", call. = FALSE)
}

try_gganimate <- function() {
  if (system.file(package = "gganimate") != "") {
    if (utils::packageVersion("gganimate") > "1.0.0") {
      return(TRUE)
    }
  }
  stop(
    "This functionality requires a recent version of the gganimate package.\n",
    "Please install via devtools:\n",
    "devtools::install_github('dgrtwo/gganimate')",
    call. = FALSE
  )
  FALSE
}

str_trim <- function(str) {
  gsub("^\\s+|\\s+$", "", str)
}

format_table <- function(table, .info) {
  # format lat/lons
  vars <- names(table)
  latIDX <- grep(latPattern(), vars)
  lonIDX <- grep(lonPattern(), vars)

  if (length(latIDX) == 1)  {
    table[[latIDX]] <- as.numeric(str_trim(table[[latIDX]]))
  } else {
    warning(
      "Couldn't automatically determine latitude column. ",
      sprintf("Please name it one of the following: '%s'",
      paste(latNames(), collapse = "', '")),
      call. = FALSE
    )
  }
  if (length(lonIDX) == 1)  {
    table[[lonIDX]] <- as.numeric(str_trim(table[[lonIDX]]))
  } else {
    warning(
      "Couldn't automatically determine longitude column. ",
      sprintf("Please name it one of the following: '%s'",
      paste(lonNames(), collapse = "', '")),
      call. = FALSE
    )
  }

  # format time
  if ("time" %in% vars) {
    # TODO: will this cover all cases?
    #table[["time"]] <- as.Date(table[["time"]], origin = '1970-01-01', tz = "GMT")
    #table[["time"]] <- lubridate::as_datetime(table[["time"]], origin = '1970-01-01', tz = "GMT")
    table[["time"]] <- lubridate::as_datetime(table[["time"]], tz = "GMT")
  }

  table
}

# sf is not properly importing rgeos.
# I need to import it, but causes note if not
# referenced. Here is a dummy function.
#dummy <- function() rgeos::getScale()
# dummy2 <- function() rgdal::checkCRSArgs("+proj=laea +y_0=0 +lon_0=155 +lat_0=-90 +ellps=WGS84 +no_defs")

# This is likely WRONG, SAD!
# rescale_lims <- function(x, .info = info(attr(x, "datasetid"))) {
#
#   # lat/lon can be on different ranges (e.g. [-180, 180] vs [0, 360])
#   # put them on the usual [-180, 180]/[-90, 90] scales
#   for (i in c("latitude", "longitude")) {
#     default <- if (i == "latitude") c(-90, 90) else c(-180, 180)
#     domain <- tryCatch({
#       d <- .info$alldata[[i]]
#       rng <- d[d$attribute_name == "actual_range", "value"]
#       as.numeric(strtrim(strsplit(rng, ",")[[1]]))
#     }, error = function(e) default)
#
#     if (is.table(x)) {
#       x[[i]] <- scales::rescale(x[[i]], default, domain)
#     } else {
#       # griddap
#       x$summary$dim[[i]]$vals <- scales::rescale(
#         x$summary$dim[[i]]$vals, default, domain
#       )
#       # assumes latitude/longitude are abbreviated to lat/lon in the data frame (I think that is safe?)
#       abbr <- substr(i, 1, 3)
#       x$data[, abbr] <- scales::rescale(
#         x$data[, abbr], c(-180, 180), domain
#       )
#     }
#   }
#   x
# }

