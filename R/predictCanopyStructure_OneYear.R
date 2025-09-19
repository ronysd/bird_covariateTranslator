# predictCanopyStructure_OneYear <- function(
#     cohortFolder, pixelGroupFolder, studyAreaRas,
#     height_model, closure_model, year
# ) {
#   cohortFile <- file.path(cohortFolder, paste0("cohortData_year", year, ".rds"))
#   pgMapFile  <- file.path(pixelGroupFolder, paste0("pixelGroupMap_year", year, ".tif"))
#   if (!file.exists(cohortFile) || !file.exists(pgMapFile))
#     stop("Missing inputs for year ", year)
#   
#   cohortData    <- readRDS(cohortFile)
#   pixelGroupMap <- terra::rast(pgMapFile)
#   
#   result <- predictClosureHeightFromCohort(
#     cohortData    = cohortData,
#     pixelGroupMap = pixelGroupMap,
#     modelHeight   = height_model,
#     modelClosure  = closure_model
#   )
#   
#   # project & focal, as you already do
#   height_1km  <- terra::project(result$canopyHeight, studyAreaRas)
#   closure_1km <- terra::project(result$canopyClosure, studyAreaRas)
#   
#   height_5x5  <- terra::focal(height_1km,  w = matrix(1, 5, 5), fun = mean, na.rm = TRUE)
#   closure_5x5 <- terra::focal(closure_1km, w = matrix(1, 5, 5), fun = mean, na.rm = TRUE)
#   
#   cv <- function(x) {
#     y <- stats::na.omit(sample(x, size = min(10, length(x)), replace = FALSE))
#     if (length(y) < 2) return(NA_real_)
#     stats::sd(y) / mean(y)
#   }
#   heightcv     <- terra::aggregate(height_1km, fact = 33, fun = cv)
#   heightcv_1km <- terra::project(heightcv, studyAreaRas)
#   heightcv_5x5 <- terra::focal(heightcv_1km, w = matrix(1, 5, 5), fun = mean, na.rm = TRUE)
#   
#   list(
#     SCANFIheight_1km   = height_1km,
#     SCANFIheight_5x5   = height_5x5,
#     SCANFIheightcv_1km = heightcv_1km,
#     SCANFIheightcv_5x5 = heightcv_5x5,
#     SCANFIclosure_1km  = closure_1km,
#     SCANFIclosure_5x5  = closure_5x5
#   )
# }

predictCanopyStructure_OneYear <- function(
    cohortData, pixelGroupMap, studyAreaRas,
    height_model, closure_model, year
) {
  message("Predicting canopy structure for year: ", year)
  
  # Predict using model
  result <- predictClosureHeightFromCohort(
    cohortData    = cohortData,
    pixelGroupMap = pixelGroupMap,
    modelHeight   = height_model,
    modelClosure  = closure_model
  )
  
  # Project to 1km resolution
  height_1km  <- terra::project(result$canopyHeight, studyAreaRas)
  closure_1km <- terra::project(result$canopyClosure, studyAreaRas)
  
  # Apply 5x5 focal mean (approx 5 km smoothing)
  height_5x5  <- terra::focal(height_1km,  w = matrix(1, 5, 5), fun = mean, na.rm = TRUE)
  closure_5x5 <- terra::focal(closure_1km, w = matrix(1, 5, 5), fun = mean, na.rm = TRUE)
  #browser()
  # Calculate CV of height in 1km resolution
  cv <- function(x) {
    y <- stats::na.omit(sample(x, size = min(10, length(x)), replace = FALSE))
    if (length(y) < 2) return(NA_real_)
    stats::sd(y) / mean(y)
  }
  
  ## original function
  # cv <- function(x) {
  #   y <- na.omit(sample(x, size = min(10, length(x)), replace = FALSE))
  #   sd(y) / mean(y)
  # }
  
  heightcv     <- terra::aggregate(height_1km, fact = 33, fun = cv)
  heightcv_1km <- terra::project(heightcv, studyAreaRas)
  heightcv_5x5 <- terra::focal(heightcv_1km, w = matrix(1, 5, 5), fun = mean, na.rm = TRUE)
  
  # Return as named list for saving or future use
  return(list(
    SCANFIheight_1km   = height_1km,
    SCANFIheight_5x5   = height_5x5,
    SCANFIheightcv_1km = heightcv_1km,
    SCANFIheightcv_5x5 = heightcv_5x5,
    SCANFIclosure_1km  = closure_1km,
    SCANFIclosure_5x5  = closure_5x5
  ))
}
