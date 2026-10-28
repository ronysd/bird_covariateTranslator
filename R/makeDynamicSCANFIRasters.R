

## NEW FUNCTION with all the dynamic variables so far
makeDynamicSCANFIRasters <- function(
    cohortData,
    pixelGroupMap,
    studyAreaRas,
    sppEquiv,
    year,
    height_1km = NULL,
    closure_1km = NULL,
    modelCover = NULL,
    speciesCoverMethod = "biomassRatio"
) {
  #browser()
  cohort_dt <- data.table::as.data.table(cohortData)
  spp_dt    <- data.table::as.data.table(sppEquiv)
  
  cohort_dt[, speciesCode_chr := as.character(speciesCode)]
  
  spp_lookup <- spp_dt[, .(
    speciesCode_chr = as.character(LandR),
    EN_generic_full,
    Type
  )]
  
  # sppEquiv can contain duplicate rows per LandR code.
  # Keep one clean mapping row per LandR species code to avoid cartesian joins.
  spp_lookup <- spp_lookup[
    !is.na(speciesCode_chr) & speciesCode_chr != "",
    .SD[1],
    by = speciesCode_chr
  ]
  
  cohort_spp <- merge(
    cohort_dt,
    spp_lookup,
    by = "speciesCode_chr",
    all.x = TRUE,
    allow.cartesian = FALSE
  )
  # --------------------------------------------------
  # Optional cover weighting
  # --------------------------------------------------
  
  if (
    speciesCoverMethod == "coverWeightedBiomass" &&
    !is.null(modelCover)
  ) {
    
    cover_lookup <- unique(cohort_spp[, .(
      speciesCode,
      ecoregionGroup
    )])
    
    cover_lookup[, cover_factor := predict(
      modelCover$mod,
      newdata = cover_lookup,
      type = "response"
    )]
    
    cohort_spp <- merge(
      cohort_spp,
      cover_lookup,
      by = c("speciesCode", "ecoregionGroup"),
      all.x = TRUE
    )
    
    cohort_spp[, cover_weight := B * cover_factor]
    
  } else {
    
    cohort_spp[, cover_weight := B]
    
  }
  if (any(is.na(cohort_spp$EN_generic_full))) {
    warning(
      "Some cohort species were not matched to sppEquiv: ",
      paste(unique(cohort_spp$speciesCode_chr[is.na(cohort_spp$EN_generic_full)]), collapse = ", ")
    )
  }
  
  cv_fun <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) < 2) return(NA_real_)
    m <- mean(x, na.rm = TRUE)
    if (!is.finite(m) || m == 0) return(NA_real_)
    stats::sd(x, na.rm = TRUE) / m
  }
  
  focal5 <- function(r) {
    terra::focal(r, w = matrix(1, 5, 5), fun = mean, na.rm = TRUE)
  }
  
  zero_raster <- function(out_name) {
    r <- terra::rast(pixelGroupMap)
    terra::values(r) <- 0
    r <- terra::project(r, studyAreaRas)
    r <- terra::mask(r, studyAreaRas)
    names(r) <- out_name
    r
  }
  
  rasterize_by_pixelGroup <- function(dt, value_col, out_name) {
    pixelIndex <- data.table::as.data.table(
      terra::as.data.frame(pixelGroupMap, cells = TRUE)
    )
    
    out_dt <- dt[pixelIndex, on = "pixelGroup"]
    
    r <- terra::rast(pixelGroupMap)
    terra::values(r) <- NA_real_
    r[out_dt$cell] <- out_dt[[value_col]]
    r <- terra::project(r, studyAreaRas)
    r <- terra::mask(r, studyAreaRas)
    names(r) <- out_name
    r
  }
  
  out <- list()
  
  # --------------------------------------------------
  # Total biomass
  # LandR B is g/m2; divide by 100 to Mg/ha
  # --------------------------------------------------
  total_dt <- cohort_spp[, .(
    total_B = sum(B, na.rm = TRUE)
  ), by = pixelGroup]
  
  biomass_dt <- total_dt[, .(
    pixelGroup,
    SCANFIbiomass = total_B / 100
  )]
  
  biomass_1km <- rasterize_by_pixelGroup(
    biomass_dt,
    "SCANFIbiomass",
    paste0("SCANFIbiomass_1km_", year)
  )
  
  biomass_5x5 <- focal5(biomass_1km)
  names(biomass_5x5) <- paste0("SCANFIbiomass_5x5_", year)
  
  out[[names(biomass_1km)]] <- biomass_1km
  out[[names(biomass_5x5)]] <- biomass_5x5
  
  # --------------------------------------------------
  # Required SCANFI species percent layers
  # Hardcoded temporary list; absent species become zero rasters.
  # --------------------------------------------------
  species_name_map <- c(
    "Balsam fir"      = "SCANFIBalsamFir",
    "Black spruce"    = "SCANFIBlackSpruce",
    "Douglas-fir"     = "SCANFIDouglasFir",
    "Jack pine"       = "SCANFIJackPine",
    "Lodgepole pine"  = "SCANFILodgepolePine",
    "Ponderosa pine"  = "SCANFIPonderosaPine",
    "Tamarack"        = "SCANFITamarack",
    "White pine"      = "SCANFIWhiteRedPine",
    "Red pine"        = "SCANFIWhiteRedPine",
    "Eastern white pine" = "SCANFIWhiteRedPine",
    "Western white pine" = "SCANFIWhiteRedPine"
  )

  species_dt <- cohort_spp[
    EN_generic_full %in% names(species_name_map),
    .(species_B = sum(cover_weight, na.rm = TRUE)),
    by = .(pixelGroup, EN_generic_full)
  ]
  
  species_dt[, scanfi_name := species_name_map[EN_generic_full]]
  
  species_dt <- species_dt[
    ,
    .(species_B = sum(species_B, na.rm = TRUE)),
    by = .(pixelGroup, scanfi_name)
  ]

  total_cover_dt <- cohort_spp[, .(
    total_cover_weight = sum(cover_weight, na.rm = TRUE)
  ), by = pixelGroup]
  
  species_dt <- merge(
    species_dt,
    total_cover_dt,
    by = "pixelGroup",
    all.x = TRUE
  )
  
  species_dt[, species_pct := data.table::fifelse(
    total_cover_weight > 0,
    species_B / total_cover_weight * 100,
    0
  )]
  required_scanfi_species <- unique(unname(species_name_map))
  
  for (scanfi_nm in required_scanfi_species) {
    # modification May 22nd
    this_dt <- total_dt[, .(pixelGroup)]
    
    this_dt <- merge(
      this_dt,
      species_dt[
        scanfi_name == scanfi_nm,
        .(pixelGroup, species_pct)
      ],
      by = "pixelGroup",
      all.x = TRUE
    )
    
    this_dt[is.na(species_pct), species_pct := 0]
    # this_dt <- species_dt[
    #   scanfi_name == scanfi_nm,
    #   .(pixelGroup, species_pct)
    # ]
    
    if (nrow(this_dt) == 0) {
      r1 <- zero_raster(paste0(scanfi_nm, "_1km_", year))
    } else {
      r1 <- rasterize_by_pixelGroup(
        this_dt,
        "species_pct",
        paste0(scanfi_nm, "_1km_", year)
      )
      
      # fill pixels with no species record as zero
      #r1[is.na(r1)] <- 0
    }
    
    r5 <- focal5(r1)
    names(r5) <- paste0(scanfi_nm, "_5x5_", year)
    
    out[[names(r1)]] <- r1
    out[[names(r5)]] <- r5
  }
  
  # --------------------------------------------------
  # Conifer / deciduous percent
  # --------------------------------------------------
  type_dt <- cohort_spp[
    Type %in% c("Conifer", "Deciduous"),
    .(type_B = sum(B, na.rm = TRUE)),
    by = .(pixelGroup, Type)
  ]
  
  type_dt <- merge(type_dt, total_dt, by = "pixelGroup", all.x = TRUE)
  type_dt[, type_pct := data.table::fifelse(
    total_B > 0,
    type_B / total_B * 100,
    0
  )]
  
  for (tp in c("Conifer", "Deciduous")) {
    
    scanfi_nm <- if (tp == "Conifer") "SCANFIprcC" else "SCANFIprcD"
    
    this_dt <- type_dt[
      Type == tp,
      .(pixelGroup, type_pct)
    ]
    
    if (nrow(this_dt) == 0) {
      r1 <- zero_raster(paste0(scanfi_nm, "_1km_", year))
    } else {
      r1 <- rasterize_by_pixelGroup(
        this_dt,
        "type_pct",
        paste0(scanfi_nm, "_1km_", year)
      )
      r1[is.na(r1)] <- 0
    }
    
    r5 <- focal5(r1)
    names(r5) <- paste0(scanfi_nm, "_5x5_", year)
    
    out[[names(r1)]] <- r1
    out[[names(r5)]] <- r5
  }
  
  # --------------------------------------------------
  # Height, closure, 5x5, and height CV
  # --------------------------------------------------
  if (!is.null(height_1km)) {
    
    height_1km <- terra::project(height_1km, studyAreaRas)
    height_1km <- terra::mask(height_1km, studyAreaRas)
    names(height_1km) <- paste0("SCANFIheight_1km_", year)
    
    height_5x5 <- focal5(height_1km)
    names(height_5x5) <- paste0("SCANFIheight_5x5_", year)
    
    heightcv_agg <- terra::aggregate(
      height_1km,
      fact = 33,
      fun = cv_fun
    )
    
    heightcv_1km <- terra::project(
      heightcv_agg,
      studyAreaRas,
      method = "bilinear",
      align = TRUE
    )
    names(heightcv_1km) <- paste0("SCANFIheightcv_1km_", year)
    
    heightcv_5x5 <- focal5(heightcv_1km)
    names(heightcv_5x5) <- paste0("SCANFIheightcv_5x5_", year)
    
    out[[names(height_1km)]]   <- height_1km
    out[[names(height_5x5)]]   <- height_5x5
    out[[names(heightcv_1km)]] <- heightcv_1km
    out[[names(heightcv_5x5)]] <- heightcv_5x5
  }
  
  if (!is.null(closure_1km)) {
    
    closure_1km <- terra::project(closure_1km, studyAreaRas)
    closure_1km <- terra::mask(closure_1km, studyAreaRas)
    names(closure_1km) <- paste0("SCANFIclosure_1km_", year)
    
    closure_5x5 <- focal5(closure_1km)
    names(closure_5x5) <- paste0("SCANFIclosure_5x5_", year)
    
    out[[names(closure_1km)]] <- closure_1km
    out[[names(closure_5x5)]] <- closure_5x5
  }
  
  return(out)
}
