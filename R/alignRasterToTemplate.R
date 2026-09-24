alignRasterToTemplate <- function(
    x,
    template,
    method = c("near", "bilinear"),
    maskToTemplate = TRUE
) {
  
  method <- match.arg(method)
  
  if (!inherits(x, "SpatRaster")) {
    stop("`x` must be a SpatRaster.")
  }
  
  if (!inherits(template, "SpatRaster")) {
    stop("`template` must be a SpatRaster.")
  }
  
  template <- template[[1]]
  
  if (!terra::same.crs(x, template)) {
    
    x <- terra::project(
      x,
      template,
      method = method
    )
    
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
  
  if (!terra::compareGeom(
    x,
    template,
    stopOnError = FALSE
  )) {
    stop("Failed to align raster to template.")
  }
  
  if (maskToTemplate) {
    x <- terra::mask(x, template)
  }
  
  return(x)
}
