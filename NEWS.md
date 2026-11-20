# plotdap 1.2.1 (development)

- New function 'add_griddap_vectors()' draws arrows for vector fields such
  as winds or currents from the u/v components in a 'griddap()' result, for
  both the 'ggplot2' and 'base' methods. Arrows are block averages of the
  grid ('maxarrows'), are drawn in the correct geographic direction on any
  projection, and their length can be adjusted with 'scale'.
- New argument 'group' in 'add_tabledap()' draws tracks (gliders, drifters,
  floats, ships, tagged animals) as one line per track, ordered by time, with
  the color varying along the track. Default 'NULL' keeps the point plot.
- New function 'add_bathymetry()' adds ocean depth as contour lines
  ('type = "contour"') or shading ('type = "shade"'), fetched from ETOPO1 on
  ERDDAP for the area of the map (or a user-supplied 'griddap()' result).
- New arguments 'baseline' and 'baseline_var' in 'add_griddap()' map
  anomalies ('var' minus a number, or minus another 'griddap()' result such
  as a climatology, averaged over time and interpolated onto the same grid).
  Anomalies default to the diverging 'balance' palette with limits symmetric
  about zero; animated grids show the anomaly for each time step.
- 'add_griddap(time = "<time value>")' works again; it previously failed
  with "object ... of mode 'function' was not found".
- 'add_griddap()' with the 'ggplot2' method now starts a new fill scale when
  the plot already has one, so two grids each keep their own color bar.

- 'add_griddap()' with the 'ggplot2' method now draws grids with 'geom_raster()'
  instead of converting every cell to a polygon, which is several times faster
  and allows a much larger 'maxpixels'. New argument 'render = "polygon"'
  restores the previous behaviour.
- With 'bbox_set(interactive = TRUE)' the graticule lines are hidden when the
  map has a grid layer: 'plotly' draws the grid as a heatmap, which it always
  places beneath line traces, so the grid lines would otherwise cover the data.
- 'bbox_set()' now uses the 'xlim'/'ylim' it is given (previously they were
  checked but then ignored). Limits are in longitude/latitude and are converted
  to the map projection; longitudes on a (180, 360) scale are shifted to match
  the data.
- 'bbox_set(p, xlim, ylim)' with unnamed limits (the pre-1.1.1 form, still
  used in the vignette) works again; previously the limits went to
  'landmask' and the call failed with "the condition has length > 1".
- Map limits on projected maps are now correct when contour layers from
  'add_griddap_contours()' are present, and layers added with 'add_ggplot()'
  that are not 'sf' objects no longer cause an error when printing.

# plotdap 1.2.0

- Contour plots using the R package 'isoband'
- Vignette updated 
- Some code refactoring

# plotdap 1.1.1

 - Improved version of 'bbox_set' to create interactive plots using 'plotly:ggplotly()'
 - Some tweaks for speed improvement
 - Updated vignette

# plotdap 1.1.0

- Revised to work with changes in 'ggplot2' 
- Removed use of deprecated 'aes_string()' and 'aes_()', using 'rlang' constructs instead
- Updated the vignette

# plotdap 1.0.3

  Removed archived maptools from Suggests

# plotdap 1.0.2
  
  fixed problem with package level documentation not working properly

# plotdap 1.0.1

 Correctly removed dependency on rgeos, rgdal


# plotdap 1.0.0

 Removed direct dependency on rgeos, rgdal
 Fixed Broken URLs in DESCRIPTION, vignette

# plotdap 0.0.9

Vignette changed so it will not fail on CRAN

# plotdap 0.0.8

fixed animation bug in 'add_griddap()' not finding the correct number
of frames

# plotdap 0.0.7

* fixed problem with \dontrun example not finding rerddap::griddap()

* fixed problem in vignette to URL link to R docs

# plotdap 0.0.6
* fixed problem when no time coordinate in dataset
* 'add_tabledap()' overlays on gridded maps now work for 'ggplot2'

# plotdap 0.0.5
* fixed problem with animation in 'add_griddap()'
* fixed bad URL for sardine example
* changed to use colors in package 'cmocean' instead of in 'rerddap'

# plotdap 0.0.4
* fixed changed SODA datasetid in vignette
* fixed Namespace and Import problems

# plotdap 0.0.3
* fixed small bug in an example that was causing 'pgkdown' to fail
* fixed Namespace and Import problems

# plotdap 0.0.2
First CRAN release

