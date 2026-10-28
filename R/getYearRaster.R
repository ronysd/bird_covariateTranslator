getYearRaster <- function(rasters, variable, year, outputName = NULL) {
  
  target <- paste0(variable, "_", as.integer(year))
  
  if (!target %in% names(rasters)) {
    return(NULL)
  }
  
  r <- rasters[[target]]
  
  if (!is.null(outputName)) {
    names(r) <- outputName
  }
  
  r
}