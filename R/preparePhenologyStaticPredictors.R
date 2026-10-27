preparePhenologyStaticPredictors <- function(
    rasterToMatch,
    koppenRaster,
    elevationRaster,
    ecoregionVector,
    landcoverRaster,
    greenupBaseline,
    dormancyBaseline,
    ecoregionField = "ECOPROVINC"
) {
  
  ## ----------------------------------------
  ## Validate raster inputs
  ## ----------------------------------------
  
  raster_inputs <- list(
    koppenRaster     = koppenRaster,
    elevationRaster  = elevationRaster,
    landcoverRaster  = landcoverRaster,
    greenupBaseline  = greenupBaseline,
    dormancyBaseline = dormancyBaseline
  )
  
  for (nm in names(raster_inputs)) {
    
    if (!inherits(raster_inputs[[nm]], "SpatRaster")) {
      stop("`", nm, "` must be a terra SpatRaster.")
    }
  }
  
  if (!inherits(rasterToMatch, "SpatRaster")) {
    stop("`rasterToMatch` must be a terra SpatRaster.")
  }
  
  
  ## ----------------------------------------
  ## 1. Köppen climate class
  ## categorical → nearest neighbour
  ## ----------------------------------------
  
  koppen <- alignRasterToTemplate(
    x        = koppenRaster,
    template = rasterToMatch,
    method   = "near"
  )
  
  names(koppen) <- "koppen_code"
  
  
  ## ----------------------------------------
  ## 2. Elevation
  ## continuous → bilinear
  ## ----------------------------------------
  
  elevation <- alignRasterToTemplate(
    x        = elevationRaster,
    template = rasterToMatch,
    method   = "bilinear"
  )
  
  names(elevation) <- "elevation"
  
  
  ## ----------------------------------------
  ## 3. Land cover
  ## already processed by bird_dataPrep
  ## categorical → nearest neighbour
  ## ----------------------------------------
  
  landcover <- alignRasterToTemplate(
    x        = landcoverRaster,
    template = rasterToMatch,
    method   = "near"
  )
  
  names(landcover) <- "landcover_code"
  
  
  ## ----------------------------------------
  ## 4. Greenup baseline
  ## continuous → bilinear
  ## ----------------------------------------
  
  greenup_baseline <- alignRasterToTemplate(
    x        = greenupBaseline,
    template = rasterToMatch,
    method   = "bilinear"
  )
  
  names(greenup_baseline) <- "greenup_baseline"
  
  
  ## ----------------------------------------
  ## 5. Dormancy baseline
  ## continuous → bilinear
  ## ----------------------------------------
  
  dormancy_baseline <- alignRasterToTemplate(
    x        = dormancyBaseline,
    template = rasterToMatch,
    method   = "bilinear"
  )
  
  names(dormancy_baseline) <- "dormancy_baseline"
  
  
  ## ----------------------------------------
  ## 6. Ecoprovince
  ## vector → rasterized directly to target grid
  ## ----------------------------------------
  
  eco <- preparePhenologyEcoregion(
    ecoregionVector = ecoregionVector,
    rasterToMatch   = rasterToMatch,
    field           = ecoregionField
  )
  
  
  ## ----------------------------------------
  ## 7. Mask raster predictors using
  ## rasterToMatch NA footprint
  ## ----------------------------------------
  
  koppen <- terra::mask(
    koppen,
    rasterToMatch
  )
  
  elevation <- terra::mask(
    elevation,
    rasterToMatch
  )
  
  landcover <- terra::mask(
    landcover,
    rasterToMatch
  )
  
  greenup_baseline <- terra::mask(
    greenup_baseline,
    rasterToMatch
  )
  
  dormancy_baseline <- terra::mask(
    dormancy_baseline,
    rasterToMatch
  )
  
  eco$raster <- terra::mask(
    eco$raster,
    rasterToMatch
  )
  
  
  ## ----------------------------------------
  ## 8. Final geometry check
  ## ----------------------------------------
  
  check_rasters <- list(
    koppen            = koppen,
    elevation         = elevation,
    landcover         = landcover,
    greenup_baseline  = greenup_baseline,
    dormancy_baseline = dormancy_baseline,
    ecoregion         = eco$raster
  )
  
  geometry_ok <- vapply(
    check_rasters,
    function(x) {
      terra::compareGeom(
        x,
        rasterToMatch,
        stopOnError = FALSE
      )
    },
    logical(1)
  )
  
  if (!all(geometry_ok)) {
    stop(
      "Some phenology predictors do not match rasterToMatch: ",
      paste(names(geometry_ok)[!geometry_ok], collapse = ", ")
    )
  }
  
  
  ## ----------------------------------------
  ## Return prepared static/reference inputs
  ## ----------------------------------------
  
  return(
    list(
      koppen            = koppen,
      elevation         = elevation,
      landcover         = landcover,
      greenup_baseline  = greenup_baseline,
      dormancy_baseline = dormancy_baseline,
      ecoregion         = eco$raster,
      ecoregion_lookup  = eco$lookup
    )
  )
}