# extractAvailableVariables_dynamic <- function(outSim) {
#   
#   ## NEW ADDITION
#   browser()
#   # climate naming: year#### only
#   if (all(grepl("^year\\d{4}$", names(raster_obj)))) {
#     years <- as.integer(gsub("year", "", names(raster_obj)))
#     return(data.frame(
#       source = path_prefix,
#       full = names(raster_obj),
#       base = basename(path_prefix),   # climate variable name
#       year = years,
#       moduleSource = moduleSource,
#       stringsAsFactors = FALSE
#     ))
#   }
#   
#   ## NEW ADDITION END
#   
#   
#   
#   current_year <- time(outSim)
#   extract_from_raster <- function(raster_obj, path_prefix, moduleSource) {
#     if (inherits(raster_obj, "SpatRaster")) {
#       layer_names <- names(raster_obj)
#       matches <- stringr::str_match(layer_names, "^(.+?)_?(\\d{4})?$")
#       data.frame(
#         source = path_prefix,
#         full = layer_names,
#         base = matches[, 2],
#         year = as.integer(matches[, 3]),
#         moduleSource = moduleSource,
#         stringsAsFactors = FALSE
#       )
#     } else NULL
#   }
#   
#   extract_from_list <- function(obj, path = "outSim", moduleSource = "unknown") {
#     result <- list()
#     for (nm in names(obj)) {
#       new_path <- paste0(path, "$", nm)
#       val <- obj[[nm]]
#       if (inherits(val, "SpatRaster")) {
#         result[[new_path]] <- extract_from_raster(val, new_path, moduleSource)
#       } else if (is.list(val)) {
#         result[[new_path]] <- extract_from_list(val, new_path, moduleSource)
#       }
#     }
#     if (length(result)) do.call(rbind, result) else NULL
#   }
#   
#   # safely check existing sections 
#   sources_to_check <- list()
#   if (!is.null(outSim$match))
#     sources_to_check$match <- list(data = outSim$match, tag = "bird_dataPrep")
#   if (!is.null(outSim$static))
#     sources_to_check$static <- list(data = outSim$static, tag = "bird_dataPrep")
#   if (!is.null(outSim$dynamic))
#     sources_to_check$dynamic <- list(data = outSim$dynamic, tag = "bird_covariateTranslator")
#   ###### NEW ADDITION
#   # ---------------------------
#   # Climate: historical
#   # ---------------------------
#   if (!is.null(outSim$historicalClimateRasters)) {
#     hist_list <- lapply(names(outSim$historicalClimateRasters), function(varName) {
#       yrs <- names(outSim$historicalClimateRasters[[varName]])
#       data.frame(
#         source = "historicalClimate",
#         full   = yrs,
#         base   = varName,
#         year   = as.integer(gsub("year", "", yrs)),
#         moduleSource = "climateModule",
#         stringsAsFactors = FALSE
#       )
#     })
#     climate_hist <- do.call(rbind, hist_list)
#   }
#   
#   # ---------------------------
#   # Climate: projected
#   # ---------------------------
#   if (!is.null(outSim$projectedClimateRasters)) {
#     proj_list <- lapply(names(outSim$projectedClimateRasters), function(varName) {
#       yrs <- names(outSim$projectedClimateRasters[[varName]])
#       data.frame(
#         source = "projectedClimate",
#         full   = yrs,
#         base   = varName,
#         year   = as.integer(gsub("year", "", yrs)),
#         moduleSource = "climateModule",
#         stringsAsFactors = FALSE
#       )
#     })
#     climate_proj <- do.call(rbind, proj_list)
#   }
#   
#   
#   
#   ##### NEW ADDITION
#   
#   if (length(sources_to_check) == 0) {
#     warning("No match/static/dynamic/climate sections found in outSim.")
#     return(data.frame())
#   }
#   if (exists("climate_hist")) sources_to_check$climate_hist <- list(data = climate_hist, tag = "climateModule")
#   if (exists("climate_proj")) sources_to_check$climate_proj <- list(data = climate_proj, tag = "climateModule")
#   
#   final <- do.call(rbind, lapply(names(sources_to_check), function(section) {
#     extract_from_list(
#       obj = sources_to_check[[section]]$data,
#       path = paste0("outSim$", section),
#       moduleSource = sources_to_check[[section]]$tag
#     )
#   }))
#   
#   if ("outSim$dynamic" %in% final$source) {
#     final <- final %>%
#       dplyr::filter(!(grepl("outSim\\$dynamic", source)) |
#                       grepl(paste0("_", current_year, "$"), full))
#   }
#   
# # add synthetic “smt” copies for sm-only climate variables into the meta table
# 
#   sm_vars <- final |>
#     dplyr::filter(base %in% c("ERAPPTsm_1km", "ERATavesm_1km"))   # manually specify sm variables
#   
#   # only check sm_vars for current year
#   sm_vars <- sm_vars[sm_vars$year == current_year, ]
#   
#   # if (nrow(sm_vars) > 0) {
#   #   message("Creating synthetic 'smt' versions for sm-only variables...")
#   #   smt_additions <- sm_vars |>
#   #     dplyr::mutate(base = gsub("sm", "smt", base))   # same year, same raster
#   #   final <- dplyr::bind_rows(final, smt_additions)
#   # }
#   
#   # New condition: add ONLY if smt not already in final
#   existing_smt <- final$base %in% c("ERAPPTsmt_1km", "ERATavesmt_1km")
#   
#   if (nrow(sm_vars) > 0 && !any(existing_smt)) {
#     message("Creating synthetic 'smt' versions for sm-only variables...")
#     smt_additions <- sm_vars |>
#       dplyr::mutate(base = gsub("sm", "smt", base))
#     final <- dplyr::bind_rows(final, smt_additions)
#   }
#   
#   rownames(final) <- NULL
#   return(final)
# }


################### NEW VERSION



extractAvailableVariables_dynamic <- function(outSim) {
  #browser() 
  current_year <- time(outSim)
  
  ## extract from a simlist (static/match/dynamic)

  # extract_from_raster <- function(raster_obj, path_prefix, moduleSource) {
  #   
  #   if (!inherits(raster_obj, "SpatRaster")) return(NULL)
  #   
  #   layer_names <- names(raster_obj)
  #   
  #   # match like SCANFIheight_1km_2020 or ERAPPTsm_1km_2005
  #   matches <- stringr::str_match(layer_names, "^(.+?)_?(\\d{4})?$")
  #   
  #   data.frame(
  #     source       = path_prefix,
  #     full         = layer_names,
  #     base         = matches[, 2],
  #     year         = as.integer(matches[, 3]),
  #     moduleSource = moduleSource,
  #     stringsAsFactors = FALSE
  #   )
  # }
  
  ## corrrection 13 May
  extract_from_raster <- function(raster_obj, path_prefix, moduleSource) {
    
    if (!inherits(raster_obj, "SpatRaster")) return(NULL)
    
    layer_names <- names(raster_obj)
    
    # match like SCANFIheight_1km_2020 or ERAPPTsm_1km_2005
    matches <- stringr::str_match(layer_names, "^(.+?)_?(\\d{4})?$")
    
    base_name <- matches[, 2]
    year_val  <- as.integer(matches[, 3])
    
    data.frame(
      source        = path_prefix,
      full          = layer_names,
      source_var    = base_name,
      base          = base_name,
      year          = year_val,
      moduleSource  = moduleSource,
      bird_label    = base_name,
      canopy_label  = NA_character_,
      stringsAsFactors = FALSE
    )
  }

  ## recursively extract from lists of SpatRasters
  ## (currently static/match/dynamic only)
  extract_from_list <- function(obj, path = "outSim", moduleSource = "unknown") {
    result <- list()
    
    for (nm in names(obj)) {
      new_path <- paste0(path, "$", nm)
      val <- obj[[nm]]
      
      if (inherits(val, "SpatRaster")) {
        result[[new_path]] <- extract_from_raster(val, new_path, moduleSource)
        
      } else if (is.list(val)) {
        # recursive
        result[[new_path]] <- extract_from_list(val, new_path, moduleSource)
      }
    }
    
    if (length(result)) do.call(rbind, result) else NULL
  }
  

  ## Identify sources (static, dynamic, match)
  sources_to_check <- list()
  
  if (!is.null(outSim$match))
    sources_to_check$match <- list(data = outSim$match, tag = "bird_dataPrep")
  
  if (!is.null(outSim$static))
    sources_to_check$static <- list(data = outSim$static, tag = "bird_dataPrep")
  
  if (!is.null(outSim$dynamic))
    sources_to_check$dynamic <- list(data = outSim$dynamic, tag = "bird_covariateTranslator")
  
  
  ## Extract static/match/dynamic
  final <- NULL
  if (length(sources_to_check) > 0) {
    final <- do.call(rbind, lapply(names(sources_to_check), function(section) {
      extract_from_list(
        obj          = sources_to_check[[section]]$data,
        path         = paste0("outSim$", section),
        moduleSource = sources_to_check[[section]]$tag
      )
    }))
  }
  
  #browser()  

  ## ADD CLIMATE: historicalClimateRasters
  # if (!is.null(outSim$historicalClimateRasters)) {
  #   
  #   climate_hist <- do.call(rbind, lapply(names(outSim$historicalClimateRasters), function(varName) {
  #     
  #     yrs <- names(outSim$historicalClimateRasters[[varName]])
  #     
  #     data.frame(
  #       #source       = "historicalClimate",
  #       source       = paste0("outSim$historicalClimateRasters$", varName),
  #       full         = yrs,
  #       base         = paste0(varName, "_1km"),
  #       #base         = varName,
  #       year         = as.integer(gsub("year", "", yrs)),
  #       moduleSource = "climateModule",
  #       stringsAsFactors = FALSE
  #     )
  #   }))
  #   
  #   final <- rbind(final, climate_hist)
  # }
  
  ## correction 13th may
  ## ADD CLIMATE: historicalClimateRasters
  if (!is.null(outSim$historicalClimateRasters)) {
    
    climate_hist <- do.call(rbind, lapply(names(outSim$historicalClimateRasters), function(varName) {
      
      yrs <- names(outSim$historicalClimateRasters[[varName]])
      
      data.frame(
        source        = paste0("outSim$historicalClimateRasters$", varName),
        full          = yrs,
        source_var    = varName,
        base          = varName,
        year          = as.integer(gsub("year", "", yrs)),
        moduleSource  = "historicalClimateRasters",
        Category      = "Annual Climate",
        bird_label    = paste0(varName, "_1km"),
        canopy_label  = varName,
        stringsAsFactors = FALSE
      )
    }))
    
    final <- dplyr::bind_rows(final, climate_hist)
  }

  ## ADD CLIMATE: projectedClimateRasters
  # if (!is.null(outSim$projectedClimateRasters)) {
  #   
  #   climate_proj <- do.call(rbind, lapply(names(outSim$projectedClimateRasters), function(varName) {
  #     
  #     yrs <- names(outSim$projectedClimateRasters[[varName]])
  #     
  #     data.frame(
  #       #source       = "projectedClimate",
  #       source       = paste0("outSim$projectedClimateRasters$", varName),
  #       full         = yrs,
  #       base         = paste0(varName, "_1km"),
  #       #base         = varName,
  #       year         = as.integer(gsub("year", "", yrs)),
  #       moduleSource = "climateModule",
  #       stringsAsFactors = FALSE
  #     )
  #   }))
  #   
  #   final <- rbind(final, climate_proj)
  # }
  ## correction 13th may
  ## ADD CLIMATE: projectedClimateRasters
  if (!is.null(outSim$projectedClimateRasters)) {
    
    climate_proj <- do.call(rbind, lapply(names(outSim$projectedClimateRasters), function(varName) {
      
      yrs <- names(outSim$projectedClimateRasters[[varName]])
      
      data.frame(
        source        = paste0("outSim$projectedClimateRasters$", varName),
        full          = yrs,
        source_var    = varName,
        base          = varName,
        year          = as.integer(gsub("year", "", yrs)),
        moduleSource  = "projectedClimateRasters",
        Category      = "Annual Climate",
        bird_label    = paste0(varName, "_1km"),
        canopy_label  = varName,
        stringsAsFactors = FALSE
      )
    }))
    
    final <- dplyr::bind_rows(final, climate_proj)
  }

  ## keep only current-year dynamic variables
  if (!is.null(final)) {
    final <- final |>
      dplyr::filter(!(grepl("outSim\\$dynamic", source)) |
                      year == current_year)
  }
  
  
  ## create SYNTHETIC smt VARIABLES (if needed)
## correction 13th May
  
  
  # sm_only_vars <- c("ERAPPTsm_1km", "ERATavesm_1km")
  # 
  # sm_vars <- final |>
  #   dplyr::filter(base %in% sm_only_vars, year == current_year)
  # 
  # # if already created
  # existing_smt <- final$base %in% c("ERAPPTsmt_1km", "ERATavesmt_1km")
  # 
  # if (nrow(sm_vars) > 0 && !any(existing_smt)) {
  #   message("Creating synthetic 'smt' versions for sm-only variables...")
  #   
  #   smt_additions <- sm_vars |>
  #     dplyr::mutate(base = gsub("sm", "smt", base))
  #   
  #   final <- dplyr::bind_rows(final, smt_additions)
  #   final$base_for_match <- final$base   # start identical
  #   
  #   # only climate normals lose the _1km for matching
  #   final$base_for_match[final$moduleSource == "climateNormal"] <-
  #     gsub("_1km$", "", final$base_for_match[final$moduleSource == "climateNormal"])
  #   
  # }
  ## Do not create synthetic smt variables here.
  ## Those are bird-model aliases and are handled inside buildRasterStackAnnual()
  ## through annual_climate_map + lag_df.
  
  #rownames(final) <- NULL
  return(final)
}

