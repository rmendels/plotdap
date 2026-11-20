# plotdap 1.2.1 (development)

- 'add_griddap()' with the 'ggplot2' method now draws grids with 'geom_raster()'
  instead of converting every cell to a polygon, which is several times faster
  and allows a much larger 'maxpixels'. New argument 'render = "polygon"'
  restores the previous behaviour.
- 'bbox_set()' now uses the 'xlim'/'ylim' it is given (previously they were
  checked but then ignored). Limits are in longitude/latitude and are converted
  to the map projection; longitudes on a (180, 360) scale are shifted to match
  the data.
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

