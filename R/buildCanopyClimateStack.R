
### NEW FUNCTION MAY 2026
buildCanopyClimateStack <- function(
    sim,
    year,
    canopy_predictors,
    varmetaTable
) {
  library(terra)
  library(dplyr)
browser()  
  climate_vars <- canopy_predictors[
    !grepl("^ECOPROVINC", canopy_predictors) &
      !canopy_predictors %in% c("stand_age", "biomass", "broadleaf_prop")
  ]
  
  rasters <- list()
  
  for (v in climate_vars) {
 
    # correction 13th May   
    # meta_row <- varmetaTable |>
    #   dplyr::filter(
    #     base == v,
    #     year == !!year,
    #     moduleSource %in% c("historicalClimateRasters", "projectedClimateRasters")
    #   )
    meta_row <- varmetaTable |>
      dplyr::filter(
        canopy_label == v,
        year == !!year,
        moduleSource %in% c("historicalClimateRasters", "projectedClimateRasters")
      )
    
    if (nrow(meta_row) == 0) {
      stop(
        "No climate metadata found for predictor '", v,
        "' in climate year ", year,
        ". Check varmetaTable and extractAvailableVariables()."
      )
    }
    
    # If both historical/projected exist for same year, prefer historical.
    meta_row <- meta_row |>
      dplyr::mutate(
        priority = dplyr::case_when(
          moduleSource == "historicalClimateRasters" ~ 1,
          moduleSource == "projectedClimateRasters" ~ 2,
          TRUE ~ 99
        )
      ) |>
      dplyr::arrange(priority)
    ## correction 13th may
    
    # source_var <- meta_row$base[1]
    # layer_name <- meta_row$full[1]
    # source_type <- meta_row$moduleSource[1]
    source_var <- meta_row$source_var[1]
    layer_name <- meta_row$full[1]
    source_type <- meta_row$moduleSource[1]
    
    climate_src <- switch(
      source_type,
      historicalClimateRasters = sim$historicalClimateRasters,
      projectedClimateRasters  = sim$projectedClimateRasters,
      stop("Unknown climate moduleSource: ", source_type)
    )
    
    if (!source_var %in% names(climate_src)) {
      stop(
        "Climate source variable '", source_var,
        "' not found in sim$", source_type,
        " for model predictor '", v, "'."
      )
    }
    
    if (!layer_name %in% names(climate_src[[source_var]])) {
      stop(
        "Climate layer '", layer_name,
        "' not found in sim$", source_type, "$", source_var,
        " for model predictor '", v, "'."
      )
    }
    
    r <- climate_src[[source_var]][[layer_name]]
    names(r) <- v
    rasters[[v]] <- r
  }
  
  clim_stack <- terra::rast(rasters)
  
  stopifnot(
    identical(
      sort(names(clim_stack)),
      sort(climate_vars)
    )
  )
  
  return(clim_stack)
}
