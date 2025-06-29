#' @import rerddap
#' @import mapdata
#' @importFrom utils packageVersion getFromNamespace
#' @importFrom stats quantile setNames
#' @importFrom tidyr gather_
#' @importFrom dplyr arrange between
#' @importFrom maps map
#' @importFrom scales rescale col_numeric
#' @importFrom rlang .data
#' @importFrom sf st_as_sf st_transform st_crs st_bbox st_graticule
#' @importFrom ggplot2  ggplot geom_sf guides guide_colorbar coord_sf theme_bw scale_colour_gradientn
#' scale_fill_gradientn ggtitle labs
#' @importFrom raster raster nlayers calc ncell nrow ncol crs extent resample
#' projectRaster rasterToPolygons values plot brick setExtent isLonLat
NULL

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`
