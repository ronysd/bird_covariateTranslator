preparePhenologyPredictionData <- function(
    phenologyClimate,
    phenologyStatic,
    rasterToMatch,
    model,
    type = c("greenup", "dormancy")
) {
  
  type <- match.arg(type)
  
  if (!inherits(rasterToMatch, "SpatRaster")) {
    stop("`rasterToMatch` must be a terra SpatRaster.")
  }
  
  if (!inherits(model, "gam")) {
    stop("`model` must inherit from class 'gam'.")
  }
  
  
  ## ----------------------------------------
  ## Select climate stack + baseline
  ## ----------------------------------------
  
  if (type == "greenup") {
    
    climate_stack <- phenologyClimate$greenup
    baseline_raster <- phenologyStatic$greenup_baseline
    baseline_name <- "greenup_baseline"
    response_name <- "greenup"
    
  } else {
    
    climate_stack <- phenologyClimate$dormancy
    baseline_raster <- phenologyStatic$dormancy_baseline
    baseline_name <- "dormancy_baseline"
    response_name <- "dormancy"
  }
  
  
  ## ----------------------------------------
  ## Check geometry
  ## ----------------------------------------
  
  rasters_to_check <- list(
    climate = climate_stack,
    elevation = phenologyStatic$elevation,
    landcover = phenologyStatic$landcover,
    koppen = phenologyStatic$koppen,
    ecoregion = phenologyStatic$ecoregion,
    baseline = baseline_raster
  )
  
  geometry_ok <- vapply(
    rasters_to_check,
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
      paste(
        names(geometry_ok)[!geometry_ok],
        collapse = ", "
      )
    )
  }
  
  
  ## ----------------------------------------
  ## Build predictor raster stack
  ## ----------------------------------------
  
  names(baseline_raster) <- baseline_name
  
  predictor_stack <- c(
    climate_stack,
    phenologyStatic$elevation,
    baseline_raster,
    phenologyStatic$landcover,
    phenologyStatic$koppen,
    phenologyStatic$ecoregion
  )
  
  
  ## ----------------------------------------
  ## Raster cells -> prediction data
  ## ----------------------------------------
  
  dat <- terra::as.data.frame(
    predictor_stack,
    xy = TRUE,
    cells = TRUE,
    na.rm = FALSE
  )
  
  
  ## ----------------------------------------
  ## Convert projected coordinates to lon/lat
  ##
  ## GAM was fitted using geographic
  ## longitude/latitude coordinates.
  ## ----------------------------------------
  
  pts <- terra::vect(
    dat,
    geom = c("x", "y"),
    crs = terra::crs(rasterToMatch)
  )
  
  pts_ll <- terra::project(
    pts,
    "EPSG:4326"
  )
  
  xy_ll <- terra::crds(pts_ll)
  
  dat$lon <- xy_ll[, 1]
  dat$lat <- xy_ll[, 2]
  
  
  ## ----------------------------------------
  ## Decode ecoregion
  ## ----------------------------------------
  
  eco_lookup <- phenologyStatic$ecoregion_lookup
  
  dat$ecoregion <- eco_lookup$ecoregion[
    match(
      dat$ecoregion_id,
      eco_lookup$ecoregion_id
    )
  ]
  
  eco_levels <- levels(
    model$model$ecoregion
  )
  
  unsupported_eco <- setdiff(
    unique(na.omit(dat$ecoregion)),
    eco_levels
  )
  
  if (length(unsupported_eco) > 0) {
    warning(
      "Ecoregion levels not represented in the fitted ",
      type,
      " GAM: ",
      paste(unsupported_eco, collapse = ", "),
      ". These cells will become NA."
    )
  }
  
  dat$ecoregion <- factor(
    dat$ecoregion,
    levels = eco_levels
  )
  
  
  ## ----------------------------------------
  ## Decode SCANFI landcover
  ##
  ## 1 Bryoid
  ## 2 Herbs
  ## 3 Rock
  ## 4 Shrub
  ## 5 Broadleaf
  ## 6 Conifer
  ## 7 Mixed
  ## 8 Water -> not used in GAM training
  ## ----------------------------------------
  
  landcover_lookup <- c(
    `1` = "Bryoid",
    `2` = "Herbs",
    `3` = "Rock",
    `4` = "Shrub",
    `5` = "Broadleaf",
    `6` = "Conifer",
    `7` = "Mixed",
    `8` = NA_character_
  )
  
  landcover_code_chr <- as.character(
    dat$landcover_code
  )
  
  unknown_landcover <- setdiff(
    unique(na.omit(landcover_code_chr)),
    names(landcover_lookup)
  )
  
  if (length(unknown_landcover) > 0) {
    stop(
      "Unknown landcover codes encountered: ",
      paste(unknown_landcover, collapse = ", ")
    )
  }
  
  dat$landcover <- unname(
    landcover_lookup[landcover_code_chr]
  )
  
  dat$landcover <- factor(
    dat$landcover,
    levels = model$xlevels$landcover
  )
  
  
  ## ----------------------------------------
  ## Decode Köppen
  ## ----------------------------------------
  
  dat$koppen <- factor(
    as.character(dat$koppen_code),
    levels = model$xlevels$koppen
  )
  
  
  ## ----------------------------------------
  ## Remove raw codes / projected coordinates
  ## ----------------------------------------
  
  dat$x <- NULL
  dat$y <- NULL
  dat$landcover_code <- NULL
  dat$koppen_code <- NULL
  dat$ecoregion_id <- NULL
  
  
  ## ----------------------------------------
  ## Verify all GAM predictors exist
  ## ----------------------------------------
  
  model_predictors <- setdiff(
    names(model$model),
    response_name
  )
  
  missing_predictors <- setdiff(
    model_predictors,
    names(dat)
  )
  
  if (length(missing_predictors) > 0) {
    stop(
      "Prediction data are missing GAM predictors: ",
      paste(missing_predictors, collapse = ", ")
    )
  }
  
  
  ## ----------------------------------------
  ## Put predictors in model order
  ## ----------------------------------------
  
  cells <- dat$cell
  
  predictionData <- dat[
    ,
    model_predictors,
    drop = FALSE
  ]
  
  
  return(
    list(
      predictionData = predictionData,
      cells = cells
    )
  )
}