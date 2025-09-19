makeBiomassRaster <- function(cohortData, pixelGroupMap) {
  require(data.table)
  require(terra)
  
  biomass_dt <- cohortData[, .(B_MgHa = sum(B, na.rm = TRUE) / 100), by = pixelGroup]
  
  pixelIndex <- as.data.table(terra::as.data.frame(pixelGroupMap, cells = TRUE))
  pixelIndex <- biomass_dt[pixelIndex, on = "pixelGroup"]
  
  AGB <- rast(pixelGroupMap)
  AGB[pixelIndex$cell] <- pixelIndex$B_MgHa
  
  return(AGB)
}
