## eighth function, this uses the seventh function
## final step prediction


predictCanopyFromCohort <- function(
    cohortData,
    pixelGroupMap,
    studyAreaRas,
    climate_stack,
    ecoprovince_raster,
    canopyModels,
    broadleaf_species
) {
  library(terra)
  library(data.table)
  
  all_predictors <- unique(unlist(
    lapply(canopyModels, function(x) x$predictors)
  ))
  
  pred_dt <- prepareCanopyPredictors(
    cohortData          = cohortData,
    pixelGroupMap       = pixelGroupMap,
    climate_stack       = climate_stack,
    ecoprovince_raster  = ecoprovince_raster,
    broadleaf_species   = broadleaf_species,
    required_predictors = all_predictors
  )
  
  out <- list()
  
  for (nm in names(canopyModels)) {
    
    model_obj  <- canopyModels[[nm]]
    predictors <- model_obj$predictors
    
    missing_cols <- setdiff(predictors, names(pred_dt))
    if (length(missing_cols) > 0) {
      stop("Missing predictors for model ", nm, ": ",
           paste(missing_cols, collapse = ", "))
    }
    
    X <- pred_dt[, predictors, with = FALSE]
    ok <- complete.cases(X)
    
    preds <- rep(NA_real_, nrow(pred_dt))
    #browser()
    ## correction 13th May
    # preds[ok] <- predict(
    #   model_obj$model,
    #   as.matrix(X[ok])
    # )
    X_ok <- X[ok, , drop = FALSE]
    
    # # enforce predictor order from your wrapper object, not from model$feature_names
    # X_ok <- X_ok[, model_obj$predictors, drop = FALSE]
    
    X_ok <- as.data.frame(X_ok)
    X_ok[] <- lapply(X_ok, as.numeric)
    X_ok <- as.matrix(X_ok)
    # storage.mode(X_ok) <- "double"
    
    preds[ok] <- predict(model_obj$model, X_ok)
    
    r <- terra::rast(pixelGroupMap)
    pg_vals <- terra::values(pixelGroupMap)
    
    terra::values(r) <- preds[match(pg_vals, pred_dt$pixelGroup)]
    terra::crs(r) <- terra::crs(pixelGroupMap)
    
    r <- terra::project(r, studyAreaRas)
    
    names(r) <- paste0(nm, "_pred")
    out[[nm]] <- r
  }
  
  return(out)
}
