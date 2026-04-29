# buildRasterStackAllYears <- function(outSim, lag_df, vars_available, years = seq(1985, 2020, 5)) {
# 
#   stack_list <- list()
#   covariates <- unique(vars_available$base)
#   year.out <- data.frame()
# 
#   for (cov in covariates) {
#     vars_cov <- vars_available |> filter(base == cov, !is.na(year))
# 
#     if (nrow(vars_cov) == 0) {
#       year.out <- bind_rows(year.out, data.frame(base = cov, year = NA, predyear = years))
#     } else {
#       is_lagged <- cov %in% lag_df$Label
#       years_cov <- unique(vars_cov$year)
#       if (is_lagged) years_cov <- years_cov + 1
# 
#       dt <- data.table(year = years_cov, val = years_cov)
#       setattr(dt, "sorted", "year")
#       setkey(dt, year)
#       match_years <- dt[J(years), roll = "nearest"]$val
#       if (is_lagged) match_years <- match_years - 1
# 
#       year.out <- bind_rows(year.out, data.frame(base = cov, year = match_years, predyear = years))
#     }
#   }
# 
#   matched_rows <- left_join(year.out, vars_available, by = c("base", "year")) |> filter(!is.na(full))
# 
#   for (prediction_year in years) {
#     message("Building raster stack for year: ", prediction_year)
#     vars_year <- matched_rows |> filter(predyear == prediction_year)
#     loaded_rasters <- list()
# 
#     for (i in seq_len(nrow(vars_year))) {
#       row <- vars_year[i, ]
#       message(
#         "Trying: ", row$base,
#         if (!is.na(row$year)) paste0(" (year = ", row$year, ")"),
#         " from ", row$source, " using layer: ", row$full
#       )
#       lyr <- tryCatch(
#         {
#           r <- outSim
#           for (key in strsplit(row$source, "\\$")[[1]][-1]) {
#             r <- r[[key]]
#           }
#           r <- r[[row$full]]
#           names(r) <- row$base
#           r
#         },
#         error = function(e) {
#           message("Failed to extract ", row$base, " → ", e$message)
#           NULL
#         }
#       )
#       if (!is.null(lyr)) loaded_rasters[[length(loaded_rasters) + 1]] <- lyr
#     }
# 
#     if (length(loaded_rasters) == 0) next
# 
#     template_ras <- loaded_rasters[[1]]
#     method_ras <- rast(template_ras)
#     values(method_ras) <- "PC"
#     names(method_ras) <- "method"
#     year_ras <- rast(template_ras)
#     values(year_ras) <- prediction_year
#     names(year_ras) <- "year"
# 
#     stack_out <- c(method_ras, year_ras, do.call(c, loaded_rasters))
#     stack_list[[as.character(prediction_year)]] <- stack_out
# 
#     message("stack built with ", nlyr(stack_out), " layers for ", prediction_year)
#   }
#   message("Finished building stacks for ", length(stack_list), " years.")
#   return(stack_list)
# }





## Latest WORKING, has BEEN COMMENTED OUT TO MAKE IT DYNAMIC RATHER THAN DOING ALL YEARS TOGETHER
# buildRasterStackAllYears <- function(outSim, lag_df, vars_available,
#                                      predictStartYear = 1985,
#                                      predictEndYear = 2020,
#                                      predictInterval = 5) {
#   
#   stack_list <- list()
#   covariates <- unique(vars_available$base)
#   year.out <- data.frame()
#   
#   # build prediction years dynamically
#   years <- seq(predictStartYear, predictEndYear, predictInterval)
#   
#   for (cov in covariates) {
#     vars_cov <- vars_available |> dplyr::filter(base == cov, !is.na(year))
#     
#     if (nrow(vars_cov) == 0) {
#       year.out <- dplyr::bind_rows(year.out, data.frame(base = cov, year = NA, predyear = years))
#     } else {
#       
# 
#       lag_row <- lag_df |> dplyr::filter(Label == cov)
#       if (nrow(lag_row) > 0) {
#         year_shift <- lag_row$YearMatch[1]
#         message("YearMatch found for ", cov, ": shift = ", year_shift)
#       } else if (grepl("Dormancy", cov, ignore.case = TRUE) ||
#                  grepl("Greenup", cov, ignore.case = TRUE)) {
#         year_shift <- 0
#         message("Fallback lag rule for ", cov, ": shift = ", year_shift)
#       } else {
#         year_shift <- 0
#       }
#       #browser()
#       years_cov <- unique(vars_cov$year)
#       if (year_shift < 0) {
#         years_cov_adj <- years_cov - year_shift  # reverse sign for negative lags
#       } else {
#         years_cov_adj <- years_cov + year_shift
#       }
#       
#       dt <- data.table::data.table(year = years_cov_adj, val = years_cov) #years_cov_adj
#       data.table::setkey(dt, year)
#       
#       # Strict lag alignment for non-zero YearMatch, nearest fallback otherwise
#       if (!is.na(year_shift) && year_shift != 0) {
#         # Strict alignment: use shifted year only
#         match_years <- dt[data.table::J(years)]$val
#         # If lagged year missing, allow minimal fallback
#         missing_idx <- which(is.na(match_years))
#         if (length(missing_idx) > 0) {
#           message("Missing lagged year(s) for ", cov, 
#                   " → using nearest available raster for ", length(missing_idx), " year(s).")
#           match_years[missing_idx] <- dt[data.table::J(years[missing_idx]), roll = "nearest"]$val
#         }
#       } else {
#         # No lag → allow nearest neighbour match
#         match_years <- dt[data.table::J(years), roll = "nearest"]$val
#       }
#       # --- Diagnostic reporting block ---
#       if (!is.na(year_shift) && year_shift != 0) {
#         match_years <- dt[data.table::J(years), roll = "nearest"]$val
#         
#         #  reporting only for lagged variables
#         msg_tbl <- data.frame(
#           PredictionYear = years,
#           YearMatch = year_shift,
#           RasterYearUsed = match_years
#         )
#         message("\n[Lagged variable] ", cov, " (YearMatch = ", year_shift, ")")
#         print(msg_tbl)
#       } else {
#         # Non-lagged → nearest available year
#         match_years <- dt[data.table::J(years), roll = "nearest"]$val
#       }
#       year.out <- dplyr::bind_rows(
#         year.out,
#         data.frame(base = cov, year = match_years, predyear = years)
#       )
#     }
#   }
#   #browser()
#   # Join matched year info with available variable metadata
#   matched_rows <- dplyr::left_join(year.out, vars_available, by = c("base", "year")) |>
#     dplyr::filter(!is.na(full))
#   
# ##build raster stack for the years
#   for (prediction_year in years) {
#     message("Building raster stack for year: ", prediction_year)
#     vars_year <- matched_rows |> dplyr::filter(predyear == prediction_year)
#     loaded_rasters <- list()
#     
#     for (i in seq_len(nrow(vars_year))) {
#       row <- vars_year[i, ]
#       message(
#         "Trying: ", row$base,
#         if (!is.na(row$year)) paste0(" (year = ", row$year, ")"),
#         " from ", row$source, " using layer: ", row$full
#       )
#       
#       lyr <- tryCatch(
#         {
#           r <- outSim
#           for (key in strsplit(row$source, "\\$")[[1]][-1]) {
#             r <- r[[key]]
#           }
#           r <- r[[row$full]]
#           names(r) <- row$base
#           r
#         },
#         error = function(e) {
#           message("Failed to extract ", row$base, " → ", e$message)
#           NULL
#         }
#       )
#       
#       if (!is.null(lyr)) loaded_rasters[[length(loaded_rasters) + 1]] <- lyr
#     }
#     
#     if (length(loaded_rasters) == 0) next
#     
#     # Metadata layers
#     template_ras <- loaded_rasters[[1]]
#     method_ras <- terra::rast(template_ras)
#     terra::values(method_ras) <- "PC"
#     names(method_ras) <- "method"
#     
#     year_ras <- terra::rast(template_ras)
#     terra::values(year_ras) <- prediction_year
#     names(year_ras) <- "year"
#     
#     stack_out <- c(method_ras, year_ras, do.call(c, loaded_rasters))
#     stack_list[[as.character(prediction_year)]] <- stack_out
#     
#     message("Stack built with ", terra::nlyr(stack_out), " layers for ", prediction_year)
#   }
#   
#   message("Finished building stacks for ", length(stack_list), " years.")
#   return(stack_list)
# }

################################################ Final version, before the climateYear approach  #######################################

# buildRasterStackAnnual <- function(outSim, lag_df, vars_available, prediction_year) {
#   # Ensure no accidental duplicates in metadata
#   vars_available <- vars_available |>
#     dplyr::distinct(base, year, full, source, .keep_all = TRUE)
#   
#   ##for current year only
#   vars_available <- vars_available %>%
#     dplyr::filter(is.na(year) | year <= prediction_year)
#   
#   stack_list <- list()
#   covariates <- unique(vars_available$base)
#   
#   year.out <- data.frame()
#   
#   for (cov in covariates) {
#     # vars_cov <- vars_available |>
#     #   #dplyr::filter(base == cov, !is.na(year))
#     #   #dplyr::filter(base == cov, !is.na(year))
#     #   dplyr::filter(base == cov)
#     # if (nrow(vars_cov) == 0) {
#     #   # covariate has no year-specific layers; record NA mapping for this year
#     #   year.out <- dplyr::bind_rows(
#     #     year.out,
#     #     data.frame(base_for_match = cov, year = NA_integer_, predyear = prediction_year)
#     #   )
#     #   next
#     # }
#     vars_cov <- vars_available %>%
#       filter(base == cov)
#     
#     # CASE 1: Static variable (ALL years = NA)
#     if (all(is.na(vars_cov$year))) {
#       # Static covariate → same raster used for all prediction years
#       year.out <- dplyr::bind_rows(
#         year.out,
#         data.frame(
#           base    = cov,
#           year    = NA_integer_,
#           predyear = prediction_year
#         )
#       )
#       next
#     }
#     
#     # CASE 2: Year-based variable → drop yearless rows
#     vars_cov <- vars_cov %>% filter(!is.na(year))
#     #  YearMatch / lag logic 
#     lag_row <- lag_df |>
#       dplyr::filter(Label == cov)
#     
#     if (nrow(lag_row) > 0) {
#       year_shift <- lag_row$YearMatch[1]
#       message("YearMatch found for ", cov, ": shift = ", year_shift)
#     } else if (grepl("Dormancy", cov, ignore.case = TRUE) ||
#                grepl("Greenup",  cov, ignore.case = TRUE)) {
#       year_shift <- 0
#       message("Fallback lag rule for ", cov, ": shift = ", year_shift)
#     } else {
#       year_shift <- 0
#     }
#     if (is.na(year_shift)) year_shift <- 0
#     years_cov <- unique(vars_cov$year)
#     
#     if (year_shift < 0) {
#       years_cov_adj <- years_cov - year_shift  # reverse sign for negative lags
#     } else {
#       years_cov_adj <- years_cov + year_shift
#     }
#     
#     dt <- data.table::data.table(year = years_cov_adj, val = years_cov)
#     data.table::setkey(dt, year)
#     #browser()
#     # Single prediction year
#     target_year <- prediction_year
#     
#     if (!is.na(year_shift) && year_shift != 0) {
#       # Strict lag alignment first
#       match_year <- dt[data.table::J(target_year)]$val
#       
#       # Fallback to nearest neighbour if missing
#       if (is.na(match_year)) {
#         message("Missing lagged year for ", cov,
#                 " → using nearest available raster.")
#         match_year <- dt[data.table::J(target_year), roll = "nearest"]$val
#       }
#       
#       # Diagnostic for lagged variables
#       msg_tbl <- data.frame(
#         PredictionYear = target_year,
#         YearMatch      = year_shift,
#         RasterYearUsed = match_year
#       )
#       message("\n[Lagged variable] ", cov, " (YearMatch = ", year_shift, ")")
#       print(msg_tbl)
#       
#     } else {
#       # Non-lagged → nearest available year
#       match_year <- dt[data.table::J(target_year), roll = "nearest"]$val
#     }
#     
#     year.out <- dplyr::bind_rows(
#       year.out,
#       data.frame(
#         base    = cov,
#         year    = match_year,
#         predyear = target_year
#       )
#     )
#   }
#   
#   # Join matched year info with available variable metadata
#   matched_rows <- dplyr::left_join(
#     year.out,
#     vars_available,
#     by = c("base", "year")
#   ) |>
#     dplyr::filter(!is.na(full))
#   
#   # Build raster stack for this single prediction year
#   prediction_year <- as.integer(prediction_year)
#   message("Building raster stack for year: ", prediction_year)
#   #browser()
#   loaded_rasters <- list()
#   
#   for (i in seq_len(nrow(matched_rows))) {
#     row <- matched_rows[i, ]
#     
#     message(
#       "Trying: ", row$base,
#       if (!is.na(row$year)) paste0(" (year = ", row$year, ")") else "",
#       " from ", row$source, " using layer: ", row$full
#     )
#     
#     lyr <- tryCatch(
#       {
#         # Navigate nested list structure using the $-like source string
#         # r <- outSim
#         # keys <- strsplit(row$source, "\\$")[[1]][-1]
#         # for (key in keys) {
#         #   r <- r[[key]]
#         # }
#         # r <- r[[row$full]]
#         # names(r) <- row$base
#         # Navigate nested list structure
#         r <- outSim
#         keys <- strsplit(row$source, "\\$")[[1]][-1]
#         for (key in keys) {
#           r <- r[[key]]
#         }
#         r <- r[[row$full]]
# 
#         # climate naming
#         # if (row$source %in% c("historicalClimate", "projectedClimate")) {
#         # 
#         #   climName <- row$base
#         #   # enforce climate naming convention
#         #   if (!grepl("_1km$", climName)) {
#         #     climName <- paste0(climName, "_1km")
#         #   }
#         #   names(r) <- climName
#         # } else {
#         #   # regular behaviour
#         #   names(r) <- row$base
#         # }
#         if (row$moduleSource == "climateNormal" ||
#             row$moduleSource == "projectedClimate" ||
#             row$moduleSource == "historicalClimate") {
#           
#           # enforce final name for the bird model
#           names(r) <- paste0(row$base_for_match, "_1km")
#           
#         } else {
#           names(r) <- row$base
#         }
#         r
#       },
#       error = function(e) {
#         message("Failed to extract ", row$base, " → ", e$message)
#         NULL
#       }
#     )
#     
#     if (!is.null(lyr)) {
#       loaded_rasters[[length(loaded_rasters) + 1]] <- lyr
#     }
#   }
#   
#   if (length(loaded_rasters) == 0) {
#     warning("No rasters loaded for year ", prediction_year,
#             ". Returning NULL stack.")
#     return(NULL)
#   }
#   
#   # Metadata layers (method + year)
#   template_ras <- loaded_rasters[[1]]
#   
#   method_ras <- terra::rast(template_ras)
#   terra::values(method_ras) <- "PC"
#   names(method_ras) <- "method"
#   
#   year_ras <- terra::rast(template_ras)
#   terra::values(year_ras) <- prediction_year
#   names(year_ras) <- "year"
#   
#   stack_out <- c(method_ras, year_ras, do.call(c, loaded_rasters))
#   
#   message(green("Stack built with "), bold$green(terra::nlyr(stack_out)), green(" layers for "), bold$yellow(prediction_year))
#   
#   return(stack_out)
# }




### function incorporating the climateYear variable
# buildRasterStackAnnual <- function(outSim, lag_df, vars_available, prediction_year, climate_year = NULL) {
#   #browser()
#   # Ensure no accidental duplicates in metadata
#   vars_available <- vars_available |>
#     dplyr::distinct(base, year, full, source, .keep_all = TRUE)
#   
#   stack_list <- list()
#   covariates <- unique(vars_available$base)
#   
#   year.out <- data.frame()
#   
#   for (cov in covariates) {
#     vars_cov <- vars_available %>% filter(base == cov)
#     
#     # Check if climate variable and assign correct target year
#     #isClimateVar <- any(vars_cov$moduleSource %in% c("projectedClimate", "historicalClimate"))
#     isClimateVar <- any(grepl("historicalClimateRasters|projectedClimateRasters", vars_cov$source))
#     target_year <- if (isClimateVar && !is.null(climate_year)) climate_year else prediction_year
#     
#     # Filter available rasters to <= target year
#     vars_cov <- vars_cov %>% filter(is.na(year) | year <= target_year)
#     
#     # Prefer projectedClimate if available
#     if (isClimateVar) {
#       # proj_rows <- vars_cov[vars_cov$moduleSource == "projectedClimate", ]
#       # hist_rows <- vars_cov[vars_cov$moduleSource == "historicalClimate", ]
#       proj_rows <- vars_cov[grepl("projectedClimateRasters", vars_cov$source), ]
#       hist_rows <- vars_cov[grepl("historicalClimateRasters", vars_cov$source), ]
#       
#       if (nrow(proj_rows) > 0) {
#         vars_cov <- proj_rows
#         message("Using projectedClimate for ", cov)
#       } else if (nrow(hist_rows) > 0) {
#         vars_cov <- hist_rows
#         message("Using historicalClimate for ", cov)
#       } else {
#         stop("No climate data available for ", cov)
#       }
#     }
#     
#     # CASE 1: Static variable (ALL years = NA)
#     if (all(is.na(vars_cov$year))) {
#       year.out <- dplyr::bind_rows(
#         year.out,
#         data.frame(
#           base     = cov,
#           year     = NA_integer_,
#           predyear = target_year
#         )
#       )
#       next
#     }
#     
#     # CASE 2: Year-based variable
#     vars_cov <- vars_cov %>% filter(!is.na(year))
#     #  YearMatch / lag logic 
#     lag_row <- lag_df |>
#       dplyr::filter(Label == cov)
#     
#     if (nrow(lag_row) > 0) {
#       year_shift <- lag_row$YearMatch[1]
#       message("YearMatch found for ", cov, ": shift = ", year_shift)
#     } else if (grepl("Dormancy", cov, ignore.case = TRUE) ||
#                grepl("Greenup",  cov, ignore.case = TRUE)) {
#       year_shift <- 0
#       message("Fallback lag rule for ", cov, ": shift = ", year_shift)
#     } else {
#       year_shift <- 0
#     }
#     if (is.na(year_shift)) year_shift <- 0
#     years_cov <- unique(vars_cov$year)
#     
#     if (year_shift < 0) {
#       years_cov_adj <- years_cov - year_shift  # reverse sign for negative lags
#     } else {
#       years_cov_adj <- years_cov + year_shift
#     }
#     
#     dt <- data.table::data.table(year = years_cov_adj, val = years_cov)
#     data.table::setkey(dt, year)
#     #browser()
# 
#     
#     if (!is.na(year_shift) && year_shift != 0) {
#       # Strict lag alignment first
#       match_year <- dt[data.table::J(target_year)]$val
#       
#       # Fallback to nearest neighbour if missing
#       if (is.na(match_year)) {
#         message("Missing lagged year for ", cov,
#                 " → using nearest available raster.")
#         match_year <- dt[data.table::J(target_year), roll = "nearest"]$val
#       }
#       
#       # Diagnostic for lagged variables
#       msg_tbl <- data.frame(
#         PredictionYear = target_year,
#         YearMatch      = year_shift,
#         RasterYearUsed = match_year
#       )
#       message("\n[Lagged variable] ", cov, " (YearMatch = ", year_shift, ")")
#       print(msg_tbl)
#       
#     } else {
#       # Non-lagged → nearest available year
#       match_year <- dt[data.table::J(target_year), roll = "nearest"]$val
#     }
#     
#     year.out <- dplyr::bind_rows(
#       year.out,
#       data.frame(
#         base    = cov,
#         year    = match_year,
#         predyear = target_year
#       )
#     )
#   }
#   
#   # Join matched year info with available variable metadata
#   matched_rows <- dplyr::left_join(
#     year.out,
#     vars_available,
#     by = c("base", "year")
#   ) |>
#     dplyr::filter(!is.na(full))
#   
#   # Build raster stack for this single prediction year
#   #browser()
#   prediction_year <- as.integer(prediction_year)
#   message("Building raster stack for year: ", prediction_year)
#   # message("Effective climate year used: ", ifelse(is.null(climate_year), "-", climate_year))
#   effective_climate_year <- if (is.null(climate_year)) prediction_year else climate_year
#   
#   message(
#     "Effective climate year used: ",
#     effective_climate_year,
#     if (is.null(climate_year)) " (default = simulation year)" else " (climateYear override)"
#   )
#   
#   #browser()
#   loaded_rasters <- list()
#   
#   for (i in seq_len(nrow(matched_rows))) {
#     row <- matched_rows[i, ]
#     
#     message(
#       "Trying: ", row$base,
#       if (!is.na(row$year)) paste0(" (year = ", row$year, ")") else "",
#       " from ", row$source, " using layer: ", row$full
#     )
#     
#     lyr <- tryCatch(
#       {
#         r <- outSim
#         keys <- strsplit(row$source, "\\$")[[1]][-1]
#         for (key in keys) {
#           r <- r[[key]]
#         }
#         r <- r[[row$full]]
#         if (row$moduleSource == "climateNormal" ||
#             row$moduleSource == "projectedClimate" ||
#             row$moduleSource == "historicalClimate") {
#           
#           # enforce final name for the bird model
#           names(r) <- paste0(row$base_for_match, "_1km")
#           
#         } else {
#           names(r) <- row$base
#         }
#         r
#       },
#       error = function(e) {
#         message("Failed to extract ", row$base, " → ", e$message)
#         NULL
#       }
#     )
#     
#     if (!is.null(lyr)) {
#       loaded_rasters[[length(loaded_rasters) + 1]] <- lyr
#     }
#   }
#   
#   if (length(loaded_rasters) == 0) {
#     warning("No rasters loaded for year ", prediction_year,
#             ". Returning NULL stack.")
#     return(NULL)
#   }
#   
#   # Report stacking status
#   message("Stacking ", length(loaded_rasters), " rasters for ", prediction_year)
#   
#   # Metadata layers (method + year)
#   template_ras <- loaded_rasters[[1]]
#   
#   method_ras <- terra::rast(template_ras)
#   terra::values(method_ras) <- "PC"
#   names(method_ras) <- "method"
#   
#   year_ras <- terra::rast(template_ras)
#   terra::values(year_ras) <- prediction_year
#   names(year_ras) <- "year"
#   
#   stack_out <- c(method_ras, year_ras, do.call(c, loaded_rasters))
#   
#   message(green("Stack built with "), bold$green(terra::nlyr(stack_out)), green(" layers for "), bold$yellow(prediction_year))
#   
#   return(stack_out)
# }
# 




### new function with climateNormal


buildRasterStackAnnual <- function(outSim, lag_df, vars_available, prediction_year, climate_year = NULL) {
  
  # Bird-model annual climate labels -> canClimate variable names
  # Important:
  # - some bird-model variables map to the same canClimate variable
  # - lag_df determines whether they use t or t-1
  annual_climate_map <- c(
    ERAMAP_1km     = "MAP",
    ERAMAT_1km     = "MAT",
    ERAPPTsmt_1km  = "PPT_sm",   # t
    ERAPPTsm_1km   = "PPT_sm",   # t-1
    ERAPPTwt_1km   = "PPT_wt",   # t-1
    ERATavesmt_1km = "Tave_sm",  # t
    ERATavesm_1km  = "Tave_sm",  # t-1
    ERATavewt_1km  = "Tave_wt"   # t-1
  )
  
  # keep only relevant annual climate labels, not raw canClimate names
  allowed_annual_labels <- names(annual_climate_map)
  
  vars_available <- vars_available |>
    dplyr::filter(
      !(Category == "Annual Climate" & !base %in% allowed_annual_labels)
    )
  
  vars_available <- vars_available |>
    dplyr::distinct(base, year, full, source, .keep_all = TRUE)
  
  covariates <- unique(vars_available$base)
  year.out <- data.frame()
  
  for (cov in covariates) {
    
    vars_cov <- vars_available %>% dplyr::filter(base == cov)
    if (nrow(vars_cov) == 0) next
    
    is_annual_climate <- any(vars_cov$Category == "Annual Climate", na.rm = TRUE)
    is_climate_normal <- any(vars_cov$Category == "Climate Normals", na.rm = TRUE)
    
    # Annual climate follows climate_year if available, otherwise simulation year
    target_year <- if (is_annual_climate && !is.null(climate_year)) climate_year else prediction_year
    
    # --------------------------------------------------
    # CASE 1: Climate normals remain static for now
    # --------------------------------------------------
    if (is_climate_normal) {
      year.out <- dplyr::bind_rows(
        year.out,
        data.frame(
          base = cov,
          year = NA_integer_,
          predyear = target_year,
          source_override = NA_character_,
          full_override = NA_character_,
          stringsAsFactors = FALSE
        )
      )
      next
    }
    
    # --------------------------------------------------
    # CASE 2: Annual climate from canClimate yearly outputs
    # --------------------------------------------------
    if (is_annual_climate) {
      
      lag_row <- lag_df |> dplyr::filter(Label == cov)
      year_shift <- if (nrow(lag_row) > 0) lag_row$YearMatch[1] else 0
      if (is.na(year_shift)) year_shift <- 0
      
      climate_var <- annual_climate_map[[cov]]
      if (is.null(climate_var) || is.na(climate_var)) {
        stop("No annual climate mapping defined for ", cov)
      }
      
      # Example:
      # cov = ERAPPTsm_1km, YearMatch = -1, climate_year = 2015
      # -> fetch year2014 from PPT_sm
      source_year <- target_year + year_shift
      full_name <- paste0("year", source_year)
      
      hist_ok <- !is.null(outSim$historicalClimateRasters[[climate_var]]) &&
        full_name %in% names(outSim$historicalClimateRasters[[climate_var]])
      
      proj_ok <- !is.null(outSim$projectedClimateRasters[[climate_var]]) &&
        full_name %in% names(outSim$projectedClimateRasters[[climate_var]])
      
      use_source <- NULL
      if (hist_ok) {
        use_source <- paste0("outSim$historicalClimateRasters$", climate_var)
      } else if (proj_ok) {
        use_source <- paste0("outSim$projectedClimateRasters$", climate_var)
      } else {
        stop(
          "No yearly canClimate raster found for ", cov,
          " (mapped to ", climate_var, ") at year ", source_year
        )
      }
      
      message(
        "[Annual climate] ", cov,
        " | target_year = ", target_year,
        " | YearMatch = ", year_shift,
        " | source_year = ", source_year,
        " | climate_var = ", climate_var
      )
      
      year.out <- dplyr::bind_rows(
        year.out,
        data.frame(
          base = cov,   # keep the ERA-prefixed bird-model name
          year = source_year,
          predyear = target_year,
          source_override = use_source,
          full_override = full_name,
          stringsAsFactors = FALSE
        )
      )
      
      next
    }
    
    # --------------------------------------------------
    # CASE 3: Other fully static variables
    # --------------------------------------------------
    if (all(is.na(vars_cov$year))) {
      year.out <- dplyr::bind_rows(
        year.out,
        data.frame(
          base = cov,
          year = NA_integer_,
          predyear = target_year,
          source_override = NA_character_,
          full_override = NA_character_,
          stringsAsFactors = FALSE
        )
      )
      next
    }
    
    # --------------------------------------------------
    # CASE 4: Other year-based variables
    # --------------------------------------------------
    vars_cov <- vars_cov %>% dplyr::filter(!is.na(year), year <= target_year)
    
    if (nrow(vars_cov) == 0) {
      warning("No available rasters for ", cov, " up to target year ", target_year)
      next
    }
    
    lag_row <- lag_df |> dplyr::filter(Label == cov)
    
    if (nrow(lag_row) > 0) {
      year_shift <- lag_row$YearMatch[1]
      message("YearMatch found for ", cov, ": shift = ", year_shift)
    } else if (grepl("Dormancy", cov, ignore.case = TRUE) ||
               grepl("Greenup", cov, ignore.case = TRUE)) {
      year_shift <- 0
      message("Fallback lag rule for ", cov, ": shift = 0")
    } else {
      year_shift <- 0
    }
    
    if (is.na(year_shift)) year_shift <- 0
    
    years_cov <- sort(unique(vars_cov$year))
    years_cov_adj <- years_cov + year_shift
    
    dt <- data.table::data.table(
      effective_year = years_cov_adj,
      raster_year = years_cov
    )
    data.table::setkey(dt, effective_year)
    
    if (year_shift != 0) {
      match_year <- dt[data.table::J(target_year)]$raster_year
      if (is.na(match_year)) {
        message("Missing lagged year for ", cov, " -> using nearest available raster.")
        match_year <- dt[data.table::J(target_year), roll = "nearest"]$raster_year
      }
    } else {
      match_year <- dt[data.table::J(target_year), roll = "nearest"]$raster_year
    }
    
    year.out <- dplyr::bind_rows(
      year.out,
      data.frame(
        base = cov,
        year = match_year,
        predyear = target_year,
        source_override = NA_character_,
        full_override = NA_character_,
        stringsAsFactors = FALSE
      )
    )
  }
  
  matched_rows <- dplyr::left_join(
    year.out,
    vars_available,
    by = c("base", "year")
  ) |>
    dplyr::mutate(
      source_final = dplyr::coalesce(source_override, source),
      full_final   = dplyr::coalesce(full_override, full)
    ) |>
    dplyr::filter(!is.na(full_final))
  
  prediction_year <- as.integer(prediction_year)
  effective_climate_year <- if (is.null(climate_year)) prediction_year else climate_year
  
  message("Building raster stack for simulation year: ", prediction_year)
  message(
    "Effective climate year used: ",
    effective_climate_year,
    if (is.null(climate_year)) " (default = simulation year)" else " (climateYear override)"
  )
  
  loaded_rasters <- list()
  
  for (i in seq_len(nrow(matched_rows))) {
    row <- matched_rows[i, ]
    
    message(
      "Trying: ", row$base,
      if (!is.na(row$year)) paste0(" (year = ", row$year, ")") else " (static)",
      " from ", row$source_final,
      " using layer: ", row$full_final
    )
    
    lyr <- tryCatch(
      {
        r <- outSim
        keys <- strsplit(row$source_final, "\\$")[[1]][-1]
        for (key in keys) {
          r <- r[[key]]
        }
        r <- r[[row$full_final]]
        
        # Critical: rename to bird-model expected name
        names(r) <- row$base
        
        r
      },
      error = function(e) {
        message("Failed to extract ", row$base, " -> ", e$message)
        NULL
      }
    )
    
    if (!is.null(lyr)) {
      loaded_rasters[[length(loaded_rasters) + 1]] <- lyr
    }
  }
  
  if (length(loaded_rasters) == 0) {
    warning("No rasters loaded for simulation year ", prediction_year, ". Returning NULL stack.")
    return(NULL)
  }
  
  template_ras <- loaded_rasters[[1]]
  
  method_ras <- terra::rast(template_ras)
  terra::values(method_ras) <- "PC"
  names(method_ras) <- "method"
  
  year_ras <- terra::rast(template_ras)
  terra::values(year_ras) <- prediction_year
  names(year_ras) <- "year"
  
  stack_out <- c(method_ras, year_ras, do.call(c, loaded_rasters))
  
  message(
    green("Stack built with "),
    bold$green(terra::nlyr(stack_out)),
    green(" layers for simulation year "),
    bold$yellow(prediction_year)
  )
  
  return(stack_out)
}
