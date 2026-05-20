### get climate layers needed for canopy modeling
getClimateRasterSource <- function(sim, year) {
  
  yr_name <- paste0("year", year)
  
  # check historical first
  if (!is.null(sim$historicalClimateRasters)) {
    for (v in names(sim$historicalClimateRasters)) {
      if (yr_name %in% names(sim$historicalClimateRasters[[v]])) {
        return(sim$historicalClimateRasters)
      }
    }
  }
  
  # then projected
  if (!is.null(sim$projectedClimateRasters)) {
    for (v in names(sim$projectedClimateRasters)) {
      if (yr_name %in% names(sim$projectedClimateRasters[[v]])) {
        return(sim$projectedClimateRasters)
      }
    }
  }
  
  stop("Year ", year, " not found in either historical or projected climate rasters")
}
