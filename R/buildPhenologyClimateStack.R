buildPhenologyClimateStack <- function(
    sim,
    climateYear,
    rasterToMatch,
    varmetaTable
) {
  
  ## ----------------------------------------
  ## Validate inputs
  ## ----------------------------------------
  
  climateYear <- as.integer(climateYear)
  
  if (length(climateYear) != 1L || is.na(climateYear)) {
    stop("`climateYear` must be a single valid year.")
  }
  
  if (!inherits(rasterToMatch, "SpatRaster")) {
    stop("`rasterToMatch` must be a terra SpatRaster.")
  }
  
  if (!is.data.frame(varmetaTable)) {
    stop("`varmetaTable` must be a data.frame.")
  }
  
  required_cols <- c(
    "source_var",
    "year",
    "source",
    "full",
    "moduleSource"
  )
  
  missing_cols <- setdiff(
    required_cols,
    names(varmetaTable)
  )
  
  if (length(missing_cols) > 0) {
    stop(
      "varmetaTable is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  
  ## ----------------------------------------
  ## canClimate variable -> GAM variable
  ## ----------------------------------------
  
  greenup_map <- c(
    DD5_sp  = "DD5sp",
    Tmin_sp = "Tminsp",
    Tmax_sp = "Tmaxsp",
    NFFD_sp = "NFFDsp",
    PPT_sp  = "PPTsp",
    CMD     = "CMD",
    Eref    = "Eref",
    FFP     = "FFP",
    bFFP    = "bFFP",
    MAT     = "MAT"
  )
  
  dormancy_map <- c(
    DD5_at  = "DD5at",
    Tmin_at = "Tminat",
    Tmax_at = "Tmaxat",
    NFFD_at = "NFFDat",
    PPT_at  = "PPTat",
    CMD     = "CMD",
    Eref    = "Eref",
    FFP     = "FFP",
    eFFP    = "eFFP",
    MAT     = "MAT"
  )
  
  
  ## ----------------------------------------
  ## Climate variables required across both
  ## phenology GAMs
  ## ----------------------------------------
  
  climate_vars <- unique(
    c(
      names(greenup_map),
      names(dormancy_map)
    )
  )
  
  
  ## ----------------------------------------
  ## Helper:
  ## retrieve one exact-climateYear raster
  ## from simList using varmetaTable
  ## ----------------------------------------
  
  getClimateRaster <- function(var) {
    
    rows <- varmetaTable |>
      dplyr::filter(
        .data$source_var == var,
        !is.na(.data$year),
        as.integer(.data$year) == climateYear,
        .data$moduleSource %in% c(
          "historicalClimateRasters",
          "projectedClimateRasters"
        )
      )
    
    
    ## --------------------------------------
    ## Exact climateYear is required
    ## --------------------------------------
    
    if (nrow(rows) == 0) {
      stop(
        "No exact-year canClimate raster found for variable '",
        var,
        "' in climateYear ",
        climateYear,
        "."
      )
    }
    
    
    ## --------------------------------------
    ## If both historical and projected
    ## versions exist for the same year,
    ## prefer historical
    ## --------------------------------------
    
    rows <- rows |>
      dplyr::mutate(
        source_priority = dplyr::case_when(
          .data$moduleSource == "historicalClimateRasters" ~ 1L,
          .data$moduleSource == "projectedClimateRasters"  ~ 2L,
          TRUE                                             ~ 3L
        )
      ) |>
      dplyr::arrange(.data$source_priority)
    
    row <- rows[1, , drop = FALSE]
    
    
    ## --------------------------------------
    ## Retrieve raster from simList
    ##
    ## Uses same source-path logic as the
    ## existing raster-stack workflow.
    ## --------------------------------------
    
    source_path <- as.character(row$source[[1]])
    full_name   <- as.character(row$full[[1]])
    
    r <- sim
    
    keys <- strsplit(
      source_path,
      "\\$"
    )[[1]]
    
    ## First component is normally "outSim"
    ## or equivalent, so remove it.
    keys <- keys[-1]
    
    for (key in keys) {
      
      if (is.null(r[[key]])) {
        stop(
          "Could not find '",
          key,
          "' while following source path: ",
          source_path
        )
      }
      
      r <- r[[key]]
    }
    
    
    ## --------------------------------------
    ## Extract actual raster
    ## --------------------------------------
    
    if (is.null(r[[full_name]])) {
      stop(
        "Could not retrieve climate raster '",
        full_name,
        "' from source ",
        source_path,
        "."
      )
    }
    
    r <- r[[full_name]]
    
    
    ## --------------------------------------
    ## Validate raster
    ## --------------------------------------
    
    if (!inherits(r, "SpatRaster")) {
      stop(
        "Climate object for variable '",
        var,
        "' in climateYear ",
        climateYear,
        " is not a terra SpatRaster."
      )
    }
    
    
    ## --------------------------------------
    ## Align to BAM prediction grid
    ##
    ## Climate predictors are continuous,
    ## therefore use bilinear interpolation.
    ## --------------------------------------
    
    r <- alignRasterToTemplate(
      x        = r,
      template = rasterToMatch,
      method   = "bilinear"
    )
    
    
    ## --------------------------------------
    ## Mask using rasterToMatch NA footprint
    ## --------------------------------------
    
    r <- terra::mask(
      r,
      rasterToMatch[[1]]
    )
    
    
    ## --------------------------------------
    ## Temporarily retain canClimate name
    ## --------------------------------------
    
    names(r) <- var
    
    return(r)
  }
  
  
  ## ----------------------------------------
  ## Retrieve all unique climate variables
  ## once
  ## ----------------------------------------
  
  climate_rasters <- lapply(
    climate_vars,
    getClimateRaster
  )
  
  names(climate_rasters) <- climate_vars
  
  
  ## ----------------------------------------
  ## Build Greenup climate stack
  ## ----------------------------------------
  
  greenup_list <- lapply(
    names(greenup_map),
    function(var) {
      
      r <- climate_rasters[[var]]
      
      names(r) <- unname(
        greenup_map[[var]]
      )
      
      r
    }
  )
  
  greenup_stack <- do.call(
    c,
    greenup_list
  )
  
  
  ## ----------------------------------------
  ## Build Dormancy climate stack
  ## ----------------------------------------
  
  dormancy_list <- lapply(
    names(dormancy_map),
    function(var) {
      
      r <- climate_rasters[[var]]
      
      names(r) <- unname(
        dormancy_map[[var]]
      )
      
      r
    }
  )
  
  dormancy_stack <- do.call(
    c,
    dormancy_list
  )
  
  
  ## ----------------------------------------
  ## Validate GAM predictor names
  ## ----------------------------------------
  
  expected_greenup <- unname(
    greenup_map
  )
  
  expected_dormancy <- unname(
    dormancy_map
  )
  
  missing_greenup <- setdiff(
    expected_greenup,
    names(greenup_stack)
  )
  
  missing_dormancy <- setdiff(
    expected_dormancy,
    names(dormancy_stack)
  )
  
  if (length(missing_greenup) > 0) {
    stop(
      "Greenup climate stack is missing predictors: ",
      paste(missing_greenup, collapse = ", ")
    )
  }
  
  if (length(missing_dormancy) > 0) {
    stop(
      "Dormancy climate stack is missing predictors: ",
      paste(missing_dormancy, collapse = ", ")
    )
  }
  
  
  ## ----------------------------------------
  ## Final geometry validation
  ## ----------------------------------------
  
  if (!terra::compareGeom(
    greenup_stack,
    rasterToMatch,
    stopOnError = FALSE
  )) {
    stop(
      "Greenup climate stack does not match rasterToMatch."
    )
  }
  
  if (!terra::compareGeom(
    dormancy_stack,
    rasterToMatch,
    stopOnError = FALSE
  )) {
    stop(
      "Dormancy climate stack does not match rasterToMatch."
    )
  }
  
  
  ## ----------------------------------------
  ## Return
  ## ----------------------------------------
  
  return(
    list(
      climateYear = climateYear,
      greenup     = greenup_stack,
      dormancy    = dormancy_stack
    )
  )
}