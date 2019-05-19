#' sardine Data
#'
#' pre-Download of sardine data in `add_tabledap()` example so that example
#' can run within CRAN Time limits
#'
#' obtained using the `rerddap` command
#'   sardines <- tabledap( 'FRDCPSTrawlLHHaulCatch',
#'      fields = c('latitude',  'longitude', 'time', '
#'          scientific_name', 'subsample_count'),
#'         'time>=2010-01-01', 'time<=2012-01-01',
#'         'scientific_name="Sardinops sagax"')
#' )
"sardines"
