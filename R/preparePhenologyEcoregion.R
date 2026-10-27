preparePhenologyEcoregion <- function(
    ecoregionVector,
    rasterToMatch,
    field = "ECOPROVINC"
) {
  
  ## ----------------------------------------
  ## Validate inputs
  ## ----------------------------------------
  
  if (!inherits(ecoregionVector, "sf")) {
    stop("`ecoregionVector` must be an sf object.")
  }
  
  if (!inherits(rasterToMatch, "SpatRaster")) {
    stop("`rasterToMatch` must be a terra SpatRaster.")
  }
  
  if (!field %in% names(ecoregionVector)) {
    stop(
      "Field '", field,
      "' not found in ecoregionVector."
    )
  }
  
  template <- rasterToMatch[[1]]
  
  
  ## ----------------------------------------
  ## Project vector to prediction-grid CRS
  ## ----------------------------------------
  
  eco <- sf::st_transform(
    ecoregionVector,
    terra::crs(template)
  )
  
  eco$ecoregion_label <- as.character(
    eco[[field]]
  )
  
  if (anyNA(eco$ecoregion_label) ||
      any(!nzchar(eco$ecoregion_label))) {
    
    stop(
      "Missing or empty ecological-region labels in field '",
      field, "'."
    )
  }
  
  
  ## ----------------------------------------
  ## Stable integer IDs
  ## ----------------------------------------
  
  eco_labels <- sort(
    unique(eco$ecoregion_label)
  )
  
  eco_lookup <- data.frame(
    ecoregion_id = seq_along(eco_labels),
    ecoregion = eco_labels,
    stringsAsFactors = FALSE
  )
  
  eco$ecoregion_id <- eco_lookup$ecoregion_id[
    match(
      eco$ecoregion_label,
      eco_lookup$ecoregion
    )
  ]
  
  
  ## ----------------------------------------
  ## Dissolve multipart polygons by region
  ## ----------------------------------------
  
  eco <- eco |>
    dplyr::group_by(
      ecoregion_label,
      ecoregion_id
    ) |>
    dplyr::summarise(
      geometry = sf::st_union(geometry),
      .groups = "drop"
    )
  
  
  ## ----------------------------------------
  ## Rasterize directly onto prediction grid
  ## ----------------------------------------
  
  eco_raster <- terra::rasterize(
    terra::vect(eco),
    template,
    field = "ecoregion_id",
    background = NA
  )
  
  names(eco_raster) <- "ecoregion_id"
  
  
  ## ----------------------------------------
  ## Mask to rasterToMatch NA footprint
  ## ----------------------------------------
  
  eco_raster <- terra::mask(
    eco_raster,
    template
  )
  
  
  ## ----------------------------------------
  ## Validate geometry
  ## ----------------------------------------
  
  if (!terra::compareGeom(
    eco_raster,
    template,
    stopOnError = FALSE
  )) {
    
    stop(
      "Ecoregion raster does not match rasterToMatch."
    )
  }
  
  
  ## ----------------------------------------
  ## Return raster + decoding table
  ## ----------------------------------------
  
  return(
    list(
      raster = eco_raster,
      lookup = eco_lookup
    )
  )
}