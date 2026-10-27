# 
# extractAvailableVariables <- function(outSim) {
#   extract_from_raster <- function(raster_obj, path_prefix) {
#     if (inherits(raster_obj, "SpatRaster")) {
#       layer_names <- names(raster_obj)
#       matches <- stringr::str_match(layer_names, "^(.+?)_?(\\d{4})?$")
#       data.frame(
#         source = path_prefix,
#         full = layer_names,
#         base = matches[, 2],
#         year = as.integer(matches[, 3]),
#         stringsAsFactors = FALSE
#       )
#     } else NULL
#   }
#   
#   extract_from_list <- function(obj, path = "outSim") {
#     result <- list()
#     for (nm in names(obj)) {
#       new_path <- paste0(path, "$", nm)
#       val <- obj[[nm]]
#       if (inherits(val, "SpatRaster")) {
#         result[[new_path]] <- extract_from_raster(val, new_path)
#       } else if (is.list(val)) {
#         result[[new_path]] <- extract_from_list(val, new_path)
#       }
#     }
#     do.call(rbind, result)
#   }
#   
#   sources_to_check <- list(
#     match = outSim$match, 
#     static = outSim$static
#     #, dynamic = outSim$dynamic for later, once translator module is up and working
#     )
#   final <- do.call(rbind, lapply(names(sources_to_check), function(section) {
#     extract_from_list(sources_to_check[[section]], path = paste0("outSim$", section))
#   }))
#   #browser()
#   smt_additions <- final |>
#     filter(base %in% c("ERAPPTsm_1km", "ERATavesm_1km")) |>
#     mutate(base = gsub("sm", "smt", base))
# 
#   final <- bind_rows(final, smt_additions)
#   rownames(final) <- NULL
#   return(final)
# }




extractAvailableVariables <- function(outSim) {
  extract_from_raster <- function(raster_obj, path_prefix, moduleSource) {
    if (inherits(raster_obj, "SpatRaster")) {
      layer_names <- names(raster_obj)
      matches <- stringr::str_match(layer_names, "^(.+?)_?(\\d{4})?$")
      data.frame(
        source = path_prefix,
        full = layer_names,
        base = matches[, 2],
        year = as.integer(matches[, 3]),
        moduleSource = moduleSource,
        stringsAsFactors = FALSE
      )
    } else NULL
  }
  
  extract_from_list <- function(obj, path = "outSim", moduleSource = "unknown") {
    result <- list()
    for (nm in names(obj)) {
      new_path <- paste0(path, "$", nm)
      val <- obj[[nm]]
      if (inherits(val, "SpatRaster")) {
        result[[new_path]] <- extract_from_raster(val, new_path, moduleSource)
      } else if (is.list(val)) {
        result[[new_path]] <- extract_from_list(val, new_path, moduleSource)
      }
    }
    if (length(result)) do.call(rbind, result) else NULL
  }
  
  # safely check existing sections 
  sources_to_check <- list()
  if (!is.null(outSim$match))
    sources_to_check$match <- list(data = outSim$match, tag = "bird_dataPrep")
  if (!is.null(outSim$static))
    sources_to_check$static <- list(data = outSim$static, tag = "bird_dataPrep")
  if (!is.null(outSim$dynamic))
    sources_to_check$dynamic <- list(data = outSim$dynamic, tag = "bird_covariateTranslator")
  ## NEW ADD FOR CLIMATE DATA
  if (!is.null(outSim$historicalClimateRasters))
    sources_to_check$historicalClimateRasters <- list(
      data = outSim$historicalClimateRasters,
      tag = "historicalClimateRasters"
    )
  
  if (!is.null(outSim$projectedClimateRasters))
    sources_to_check$projectedClimateRasters <- list(
      data = outSim$projectedClimateRasters,
      tag = "projectedClimateRasters"
    )
  
  if (length(sources_to_check) == 0) {
    warning("No match/static/dynamic sections found in outSim.")
    return(data.frame())
  }
  
  final <- do.call(rbind, lapply(names(sources_to_check), function(section) {
    extract_from_list(
      obj = sources_to_check[[section]]$data,
      path = paste0("outSim$", section),
      moduleSource = sources_to_check[[section]]$tag
    )
  }))
  

# add synthetic “smt” copies for sm-only climate variables into the meta table

  sm_vars <- final |>
    dplyr::filter(base %in% c("ERAPPTsm_1km", "ERATavesm_1km"))   # manually specify sm variables
  
  if (nrow(sm_vars) > 0) {
    message("Creating synthetic 'smt' versions for sm-only variables...")
    smt_additions <- sm_vars |>
      dplyr::mutate(base = gsub("sm", "smt", base))   # same year, same raster
    final <- dplyr::bind_rows(final, smt_additions)
  }
  
  rownames(final) <- NULL
  return(final)
}
