### latest version for prepare predictors

prepareCanopyPredictors <- function(
    cohortData,
    pixelGroupMap,
    climate_stack,
    ecoprovince_raster,
    required_predictors,
    broadleaf_species
) {
  library(data.table)
  library(terra)
  
  cohort_dt <- as.data.table(cohortData)
  
  # validate required cohort columns 
  required_cols <- c("pixelGroup", "age", "B", "speciesCode")
  missing_cols  <- setdiff(required_cols, names(cohort_dt))
  
  if (length(missing_cols) > 0) {
    stop("Cohort data missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }
  
  # aggregate cohort
  cohort_sum <- cohort_dt[, .(
    biomass   = sum(B, na.rm = TRUE),
    stand_age = max(age, na.rm = TRUE),
    broadleaf_biomass = sum(
      B[speciesCode %in% broadleaf_species],
      na.rm = TRUE
    )
  ), by = pixelGroup]
  
  cohort_sum[, broadleaf_prop :=
               fifelse(biomass > 0,
                       broadleaf_biomass / biomass,
                       0)]
  
  cohort_sum[, broadleaf_biomass := NULL]
  #browser()
  ## new addition for extent missmatch
  # Align predictors to pixelGroupMap geometry.
  # pixelGroupMap must remain the zonal raster because it contains pixelGroup IDs.
  if (!terra::compareGeom(climate_stack, pixelGroupMap, stopOnError = FALSE)) {
    climate_stack <- terra::crop(climate_stack, pixelGroupMap)
    climate_stack <- terra::resample(climate_stack, pixelGroupMap, method = "bilinear")
  }
  
  if (!terra::compareGeom(ecoprovince_raster, pixelGroupMap, stopOnError = FALSE)) {
    ecoprovince_raster <- terra::crop(ecoprovince_raster, pixelGroupMap)
    ecoprovince_raster <- terra::resample(ecoprovince_raster, pixelGroupMap, method = "near")
  }
  
  # extract climate
  clim_vals <- terra::zonal(
    climate_stack,
    pixelGroupMap,
    fun = "mean",
    na.rm = TRUE
  )
  
  clim_dt <- as.data.table(clim_vals)
  setnames(clim_dt, names(clim_dt)[1], "pixelGroup")
  
  # extract ecoprovince  (or ecoregion, when changed accordingly)
  #browser()
  eco_vals <- terra::zonal(
    ecoprovince_raster,
    pixelGroupMap,
    fun = "modal",
    na.rm = TRUE
  )
  
  ## correction 13th may
  # eco_dt <- as.data.table(eco_vals)
  # setnames(eco_dt, names(eco_dt)[1], "pixelGroup")
  # setnames(eco_dt, names(eco_dt)[2], "ECOPROVINC")
  # 
  # # map back to labels
  # lev <- levels(ecoprovince_raster)[[1]]
  # 
  # eco_dt[, ECOPROVINC := lev$ECOPROVINC[ #ECOPROVINC
  #   match(ECOPROVINC, lev$ECOPROVINC) #ECOPROVINC, lev$ID
  # ]]
  # 
  # pred_dt <- Reduce(
  #   function(x, y) merge(x, y, by = "pixelGroup", all.x = TRUE),
  #   list(cohort_sum, clim_dt, eco_dt)
  # )
  # # convert ecoprovince to factor
  # pred_dt$ECOPROVINC <- as.factor(pred_dt$ECOPROVINC) #ECOPROVINC
  # 
  # # create dummy variables ONLY for ecoprovince
  # eco_mm <- model.matrix(~ ECOPROVINC + 0, data = pred_dt) #ECOPROVINC
  # eco_dt <- as.data.table(eco_mm)
  # 
  # ## add training ecoprovinces (as Zero / 0), if they are not present in the study area
  # eco_preds <- required_predictors[grepl("^ECOPROVINC", required_predictors)] #ECOPROVINC
  # 
  # for (p in eco_preds) {
  #   if (!p %in% names(eco_dt)) {
  #     eco_dt[, (p) := 0]
  #   }
  # }
  # 
  # pred_dt[, ECOPROVINC := NULL] #ECOPROVINC
  # 
  # # append dummy variables
  # pred_dt <- cbind(pred_dt, eco_dt)
  
  eco_dt <- as.data.table(eco_vals)
  setnames(eco_dt, names(eco_dt)[1], "pixelGroup")
  
  if (ncol(eco_dt) < 2) {
    stop("Ecoprovince zonal extraction failed: no ECOPROVINC column returned.")
  }
  
  setnames(eco_dt, names(eco_dt)[2], "ECOPROVINC")
  
  # Do not use levels(ecoprovince_raster); this raster is numeric, not categorical.
  eco_dt[, ECOPROVINC := as.character(ECOPROVINC)]
  
  pred_dt <- Reduce(
    function(x, y) merge(x, y, by = "pixelGroup", all.x = TRUE),
    list(cohort_sum, clim_dt, eco_dt)
  )
  
  # Build ECOPROVINC dummy variables manually.
  # This avoids model.matrix() failing when the study area has only one ecoprovince.
  eco_preds <- required_predictors[grepl("^ECOPROVINC", required_predictors)]
  
  for (p in eco_preds) {
    pred_dt[, (p) := 0L]
  }
  
  for (p in eco_preds) {
    eco_value <- sub("^ECOPROVINC", "", p)
    pred_dt[ECOPROVINC == eco_value, (p) := 1L]
  }
  
  pred_dt[, ECOPROVINC := NULL]
  # enforce predictors 
  missing_pred <- setdiff(required_predictors, names(pred_dt))
  if (length(missing_pred) > 0) {
    stop("Missing predictors in prediction table: ",
         paste(missing_pred, collapse = ", "))
  }
  
  pred_dt <- pred_dt[, c("pixelGroup", required_predictors), with = FALSE]
  
  return(pred_dt[])
}
