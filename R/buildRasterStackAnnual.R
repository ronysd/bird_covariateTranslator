buildRasterStackAnnual <- function(outSim, lag_df, vars_available, prediction_year) {
  # Ensure no accidental duplicates in metadata
  vars_available <- vars_available |>
    dplyr::distinct(base, year, full, source, .keep_all = TRUE)
  
  ##for current year only
  vars_available <- vars_available %>%
    dplyr::filter(is.na(year) | year <= prediction_year)
  
  stack_list <- list()
  covariates <- unique(vars_available$base)
  
  year.out <- data.frame()
  
  for (cov in covariates) {
    # vars_cov <- vars_available |>
    #   #dplyr::filter(base == cov, !is.na(year))
    #   #dplyr::filter(base == cov, !is.na(year))
    #   dplyr::filter(base == cov)
    # if (nrow(vars_cov) == 0) {
    #   # covariate has no year-specific layers; record NA mapping for this year
    #   year.out <- dplyr::bind_rows(
    #     year.out,
    #     data.frame(base_for_match = cov, year = NA_integer_, predyear = prediction_year)
    #   )
    #   next
    # }
    vars_cov <- vars_available %>%
      filter(base == cov)
    
    #static variable (ALL years = NA)
    if (all(is.na(vars_cov$year))) {
      # Static covariate → same raster used for all prediction years
      year.out <- dplyr::bind_rows(
        year.out,
        data.frame(
          base    = cov,
          year    = NA_integer_,
          predyear = prediction_year
        )
      )
      next
    }
    
    # year-based variable → drop yearless rows
    vars_cov <- vars_cov %>% filter(!is.na(year))
    #  YearMatch / lag logic 
    lag_row <- lag_df |>
      dplyr::filter(Label == cov)
    
    if (nrow(lag_row) > 0) {
      year_shift <- lag_row$YearMatch[1]
      message("YearMatch found for ", cov, ": shift = ", year_shift)
    } else if (grepl("Dormancy", cov, ignore.case = TRUE) ||
               grepl("Greenup",  cov, ignore.case = TRUE)) {
      year_shift <- 0
      message("Fallback lag rule for ", cov, ": shift = ", year_shift)
    } else {
      year_shift <- 0
    }
    if (is.na(year_shift)) year_shift <- 0
    years_cov <- unique(vars_cov$year)
    
    if (year_shift < 0) {
      years_cov_adj <- years_cov - year_shift  # reverse sign for negative lags
    } else {
      years_cov_adj <- years_cov + year_shift
    }
    
    dt <- data.table::data.table(year = years_cov_adj, val = years_cov)
    data.table::setkey(dt, year)
    #browser()
    # single prediction year
    target_year <- prediction_year
    
    if (!is.na(year_shift) && year_shift != 0) {
      # strict lag alignment first
      match_year <- dt[data.table::J(target_year)]$val
      
      # Fallback to nearest neighbour if missing
      if (is.na(match_year)) {
        message("Missing lagged year for ", cov,
                " → using nearest available raster.")
        match_year <- dt[data.table::J(target_year), roll = "nearest"]$val
      }
      
      # Diagnostic for lagged variables
      msg_tbl <- data.frame(
        PredictionYear = target_year,
        YearMatch      = year_shift,
        RasterYearUsed = match_year
      )
      message("\n[Lagged variable] ", cov, " (YearMatch = ", year_shift, ")")
      print(msg_tbl)
      
    } else {
      # Non-lagged → nearest available year
      match_year <- dt[data.table::J(target_year), roll = "nearest"]$val
    }
    
    year.out <- dplyr::bind_rows(
      year.out,
      data.frame(
        base    = cov,
        year    = match_year,
        predyear = target_year
      )
    )
  }
  
  # join matched year info with available variable metadata
  matched_rows <- dplyr::left_join(
    year.out,
    vars_available,
    by = c("base", "year")
  ) |>
    dplyr::filter(!is.na(full))
  
  # Build raster stack for this single prediction year
  prediction_year <- as.integer(prediction_year)
  message("Building raster stack for year: ", prediction_year)
  #browser()
  loaded_rasters <- list()
  
  for (i in seq_len(nrow(matched_rows))) {
    row <- matched_rows[i, ]
    
    message(
      "Trying: ", row$base,
      if (!is.na(row$year)) paste0(" (year = ", row$year, ")") else "",
      " from ", row$source, " using layer: ", row$full
    )
    
    lyr <- tryCatch(
      {
        # Navigate nested list structure using the $-like source string
        # r <- outSim
        # keys <- strsplit(row$source, "\\$")[[1]][-1]
        # for (key in keys) {
        #   r <- r[[key]]
        # }
        # r <- r[[row$full]]
        # names(r) <- row$base
        # Navigate nested list structure
        r <- outSim
        keys <- strsplit(row$source, "\\$")[[1]][-1]
        for (key in keys) {
          r <- r[[key]]
        }
        r <- r[[row$full]]

        # climate naming
        # if (row$source %in% c("historicalClimate", "projectedClimate")) {
        # 
        #   climName <- row$base
        #   # enforce climate naming convention
        #   if (!grepl("_1km$", climName)) {
        #     climName <- paste0(climName, "_1km")
        #   }
        #   names(r) <- climName
        # } else {
        #   # regular behaviour
        #   names(r) <- row$base
        # }
        if (row$moduleSource == "climateNormal" ||
            row$moduleSource == "projectedClimate" ||
            row$moduleSource == "historicalClimate") {
          
          # enforce final name for the bird model
          names(r) <- paste0(row$base_for_match, "_1km")
          
        } else {
          names(r) <- row$base
        }
        r
      },
      error = function(e) {
        message("Failed to extract ", row$base, " → ", e$message)
        NULL
      }
    )
    
    if (!is.null(lyr)) {
      loaded_rasters[[length(loaded_rasters) + 1]] <- lyr
    }
  }
  
  if (length(loaded_rasters) == 0) {
    warning("No rasters loaded for year ", prediction_year,
            ". Returning NULL stack.")
    return(NULL)
  }
  
  # Metadata layers (method + year)
  template_ras <- loaded_rasters[[1]]
  
  method_ras <- terra::rast(template_ras)
  terra::values(method_ras) <- "PC"
  names(method_ras) <- "method"
  
  year_ras <- terra::rast(template_ras)
  terra::values(year_ras) <- prediction_year
  names(year_ras) <- "year"
  
  stack_out <- c(method_ras, year_ras, do.call(c, loaded_rasters))
  
  message(green("Stack built with "), bold$green(terra::nlyr(stack_out)), green(" layers for "), bold$yellow(prediction_year))
  
  return(stack_out)
}
