

## new function

predictClosureHeightFromCohort <- function(
    cohortData,
    pixelGroupMap,
    studyAreaRas,
    height_model,
    closure_model,
    year = NULL
) {
  # check models
  if (is.null(height_model) || is.null(closure_model)) {
    stop("height_model and closure_model must be provided.")
  }
  
  #  Prepare cohort data
  cohort <- copy(cohortData)
  setnames(cohort, "pixelGroup", "pixelGroupID", skip_absent = TRUE)
  
  if (!all(c("age", "B") %in% names(cohort))) {
    stop("Cohort data must have 'age' and 'B'")
  }
  
  cohort <- cohort[!is.na(B) & B > 0]
  
  cohort[, B_raw := B]
  cohort[, c("age", "B") := lapply(.SD, scale), .SDcols = c("age", "B")]
  cohort_matrix <- as.matrix(cohort[, .(age, B)])
  
  # Predict
  cohort[, predicted_closure := predict(closure_model, newdata = cohort_matrix)]
  cohort[, predicted_height  := predict(height_model,  newdata = cohort_matrix)]
  
  # Aggregate predictions per pixelGroup
  pixel_stats <- cohort[, .(
    height = max(predicted_height, na.rm = TRUE),
    closure = sum(predicted_closure * B_raw, na.rm = TRUE) / sum(B_raw, na.rm = TRUE)
  ), by = pixelGroupID]
  
  # Rasterize
  pixelIndex <- as.data.table(terra::as.data.frame(pixelGroupMap, cells = TRUE))
  setnames(pixelIndex, old = names(pixelIndex)[2], new = "pixelGroupID")
  pixelIndex <- merge(pixelIndex, pixel_stats, by = "pixelGroupID", all.x = TRUE)
  
  r_height  <- terra::rast(pixelGroupMap)
  r_closure <- terra::rast(pixelGroupMap)
  
  r_height[pixelIndex$cell]  <- pixelIndex$height
  r_closure[pixelIndex$cell] <- pixelIndex$closure
  
  names(r_height)  <- paste0("SCANFIheight_1km_", year)
  names(r_closure) <- paste0("SCANFIclosure_1km_", year)
  
  ## extent alignment with study area
  r_height  <- terra::project(r_height,  studyAreaRas, method = "near")
  r_closure <- terra::project(r_closure, studyAreaRas, method = "near")
  
  return(list(
    SCANFIheight_1km = r_height,
    SCANFIclosure_1km = r_closure
  ))
}

