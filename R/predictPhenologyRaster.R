predictPhenologyRaster <- function(
    model,
    phenologyPredictionData,
    rasterToMatch,
    type = c("greenup", "dormancy"),
    climateYear
) {
  
  type <- match.arg(type)
  climateYear <- as.integer(climateYear)
  
  if (!inherits(model, "gam")) {
    stop("`model` must inherit from class 'gam'.")
  }
  
  if (!inherits(rasterToMatch, "SpatRaster")) {
    stop("`rasterToMatch` must be a terra SpatRaster.")
  }
  
  if (
    is.null(phenologyPredictionData$predictionData) ||
    is.null(phenologyPredictionData$cells)
  ) {
    stop(
      "`phenologyPredictionData` must contain ",
      "`predictionData` and `cells`."
    )
  }
  
  
  ## ----------------------------------------
  ## GAM prediction
  ## ----------------------------------------
  
  pred <- predict(
    model,
    newdata = phenologyPredictionData$predictionData,
    type = "response"
  )
  
  if (length(pred) != length(phenologyPredictionData$cells)) {
    stop(
      "Number of predictions does not match ",
      "the number of raster cells."
    )
  }
  
  
  ## ----------------------------------------
  ## Reconstruct raster
  ## ----------------------------------------
  
  pred_values <- rep(
    NA_real_,
    terra::ncell(rasterToMatch)
  )
  
  pred_values[
    phenologyPredictionData$cells
  ] <- as.numeric(pred)
  
  pred_raster <- terra::setValues(
    rasterToMatch[[1]],
    pred_values
  )
  
  
  ## ----------------------------------------
  ## Name output using BAM convention
  ## ----------------------------------------
  
  if (type == "greenup") {
    
    names(pred_raster) <- paste0(
      "StandardGreenup_1km_",
      climateYear
    )
    
  } else {
    
    names(pred_raster) <- paste0(
      "StandardDormancy_1km_",
      climateYear
    )
  }
  
  
  return(pred_raster)
}