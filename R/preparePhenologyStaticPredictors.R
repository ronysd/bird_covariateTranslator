preparePhenologyStaticPredictors <- function(
    studyAreaRas,
    landcoverRaster,
    koppenRaster,
    elevationRaster,
    ecoregionVector,
    ecoregionField = "ECOPROVINC"
) {
  
  master <- studyAreaRas[[1]]
  
  ## -------------------------------
  ## Land cover: categorical
  ## -------------------------------
  
  landcover <- alignRasterToTemplate(
    landcoverRaster,
    master,
    method = "near"
  )
  
  names(landcover) <- "landcover_code"
  
  
  ## -------------------------------
  ## Koppen: categorical
  ## -------------------------------
  
  koppen <- alignRasterToTemplate(
    koppenRaster,
    master,
    method = "near"
  )
  
  names(koppen) <- "koppen_code"
  
  
  ## -------------------------------
  ## Elevation: continuous
  ## -------------------------------
  
  elevation <- alignRasterToTemplate(
    elevationRaster,
    master,
    method = "bilinear"
  )
  
  names(elevation) <- "elevation"
  
  
  ## -------------------------------
  ## Ecoregion: categorical
  ## -------------------------------
  
  eco <- sf::st_transform(
    ecoregionVector,
    terra::crs(master)
  )
  
  if (!ecoregionField %in% names(eco)) {
    stop(
      "Ecoregion field '",
      ecoregionField,
      "' not found."
    )
  }
  
  eco$ecoregion_label <- as.character(
    eco[[ecoregionField]]
  )
  
  eco_labels <- sort(unique(
    eco$ecoregion_label
  ))
  
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
  
  ecoregion <- terra::rasterize(
    terra::vect(eco),
    master,
    field = "ecoregion_id"
  )
  
  names(ecoregion) <- "ecoregion_id"
  
  
  ## -------------------------------
  ## Final geometry validation
  ## -------------------------------
  
  statics <- list(
    landcover = landcover,
    koppen = koppen,
    elevation = elevation,
    ecoregion = ecoregion
  )
  
  geometry_ok <- vapply(
    statics,
    function(x) {
      terra::compareGeom(
        x,
        master,
        stopOnError = FALSE
      )
    },
    logical(1)
  )
  
  if (!all(geometry_ok)) {
    stop(
      "Phenology static geometry mismatch: ",
      paste(
        names(geometry_ok)[!geometry_ok],
        collapse = ", "
      )
    )
  }
  
  return(list(
    stack = c(
      landcover,
      koppen,
      ecoregion,
      elevation
    ),
    landcover = landcover,
    koppen = koppen,
    elevation = elevation,
    ecoregion = ecoregion,
    ecoregion_lookup = eco_lookup,
    master = master
  ))
}
