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


elevation <- Cache(
  prepInputs,
  #targetFile = "cdem-canada-dem.tif",
  url = "https://drive.google.com/file/d/14puAtns8oTZDtvWzpQ6_FgK4MbozGZFK/edit",
  destinationPath = "inputs/elevation/trial/",
  fun = "terra::rast",
  overwrite = FALSE,
  userTags = c(
    "object:elevationRaw", "source:NRCan-CDEM"
  )
)
