predictCanopyStructure_MultiYear <- function(
    cohortFolder,
    pixelGroupFolder,
    studyAreaRas,
    height_model,
    closure_model,
    startYear,
    endYear,
    interval = 5
) {
  require(terra)
  require(data.table)
  outSim <- list(stack_list = list())
  
  cv <- function(x) {
    y <- na.omit(sample(x, size = min(10, length(x)), replace = FALSE))
    if (length(y) < 2) return(NA)
    return(sd(y) / mean(y))
  }
  
  for (yr in seq(startYear, endYear, by = interval)) {
    message(crayon::blue(paste("Processing year", yr)))
    
    cohortFile <- file.path(cohortFolder, paste0("cohortData_year", yr, ".rds"))
    pgMapFile  <- file.path(pixelGroupFolder, paste0("pixelGroupMap_year", yr, ".tif"))
    
    tryCatch({
      if (!file.exists(cohortFile) || !file.exists(pgMapFile))
        stop("Missing input file(s) for year ", yr)
      
      cohortData <- readRDS(cohortFile)
      pixelGroupMap <- terra::rast(pgMapFile)
      
      result <- predictClosureHeightFromCohort(
        pspData = NULL,  # unused
        cohortData = cohortData,
        pixelGroupMap = pixelGroupMap,
        studyAreaRas = studyAreaRas,
        modelHeight = height_model,
        modelClosure = closure_model
      )
      #browser()
      # Project to 1km and apply focal operations
      height_1km <- terra::project(result$canopyHeight, studyAreaRas)
      closure_1km <- terra::project(result$canopyClosure, studyAreaRas)
      
      height_5x5 <- terra::focal(height_1km, w = matrix(1, 5, 5), fun = mean, na.rm = TRUE)
      closure_5x5 <- terra::focal(closure_1km, w = matrix(1, 5, 5), fun = mean, na.rm = TRUE)
      
      heightcv <- aggregate(height_1km, fact = 33, fun = cv)
      heightcv_1km <- project(heightcv, studyAreaRas)
      heightcv_5x5 <- terra::focal(heightcv_1km, w = matrix(1, 5, 5), fun = mean, na.rm = TRUE)
      # 
      # ras_cv <- 
      # ras_cv_1km <- project(ras_cv, studyAreaRas)
      # ras_cv_5x5 <- focal(ras_cv_1km, w = matrix(1, 5, 5), fun = mean, na.rm = TRUE)
      # 
      rasList <- list(
        SCANFIheight_1km = height_1km,
        SCANFIheight_5x5 = height_5x5,
        SCANFIheightcv_1km = heightcv_1km,
        SCANFIheightcv_5x5 = heightcv_5x5,
        SCANFIclosure_1km = closure_1km,
        SCANFIclosure_5x5 = closure_5x5
      )
      
      outSim$stack_list[[as.character(yr)]] <- rasList
      message(crayon::green(paste("✓ Year", yr, "completed")))
      
    }, error = function(e) {
      message(crayon::red(paste("✗ Skipped year", yr, ":", e$message)))
    })
  }
  
  return(outSim)
}