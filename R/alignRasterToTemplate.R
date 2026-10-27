alignRasterToTemplate <- function(
    x,
    template,
    method = c("near", "bilinear")
) {
  
  method <- match.arg(method)
  
  if (!inherits(x, "SpatRaster")) {
    stop("`x` must be a terra SpatRaster.")
  }
  
  if (!inherits(template, "SpatRaster")) {
    stop("`template` must be a terra SpatRaster.")
  }
  
  ## Use only one layer to define the target geometry
  template <- template[[1]]
  
  ## --------------------------------------------------
  ## 1. Different CRS
  ## --------------------------------------------------
  ## terra::project(..., template) simultaneously:
  ##   - transforms CRS
  ##   - adopts target resolution
  ##   - adopts target origin
  ##   - adopts target extent/grid geometry
  ## --------------------------------------------------
  
  if (!terra::same.crs(x, template)) {
    
    x <- terra::project(
      x,
      template,
      method = method
    )
    
    ## --------------------------------------------------
    ## 2. Same CRS, but different grid geometry
    ## --------------------------------------------------
    
  } else if (!terra::compareGeom(
    x,
    template,
    stopOnError = FALSE
  )) {
    
    x <- terra::resample(
      x,
      template,
      method = method
    )
  }
  
  ## --------------------------------------------------
  ## 3. Final validation
  ## --------------------------------------------------
  
  if (!terra::compareGeom(
    x,
    template,
    stopOnError = FALSE
  )) {
    
    stop(
      "Failed to align raster to template geometry."
    )
  }
  
  return(x)
}