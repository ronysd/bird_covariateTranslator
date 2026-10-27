# ## This function downloads the PSP data, uses PSP data to match the closest SCANFI year (for closure and height) and extract
# ## height and closure data for the PSP locations, using a buffer around the PSP based on the plot size 
# 
# 
# prepareCanopyModelData <- function(scanfi_dir, psp_path) {
# 
#   # Download and load PSP data
#   psp_data <- getPSP(PSPdataTypes = c("BC", "QC", "ON", "NB", "AB", "NFI", "SK"), destinationPath = psp_path)
#   PSPmeasure <- as.data.table(psp_data$PSPmeasure)
#   PSPplot <- as.data.table(psp_data$PSPplot)
#   PSPgis <- st_as_sf(psp_data$PSPgis)
#   #browser()
#   # Biomass calculation
#   ## new fix for updated PSPClean Ian
#   sppEquiv <- LandR::sppEquivalencies_CA
#   sppEquiv_unq <- unique(sppEquiv, by = "Latin_full")
#   PSPmeasure <- merge(
#     PSPmeasure,
#     sppEquiv_unq,
#     by.x = "Species",
#     by.y = "Latin_full",
#     all.x = TRUE,
#     suffixes = c("", "_spp")
#   )
#   
#   PSPmeasure <- PSPmeasure[!is.na(PSP_spp)]
#   
#   PSPmeasure[, biomass := pemisc::biomassCalculation(
#     species = PSPmeasure$PSP_spp,   # or PSP (they should be identical after merge)
#     DBH = DBH,
#     includeHeight = TRUE,
#     height = Height
#   )$biomass]
#   
#   # model <- biomassCalculation(
#   #   species = PSPmeasure$newSpeciesName,
#   #   DBH = PSPmeasure$DBH,
#   #   includeHeight = TRUE,
#   #   height = PSPmeasure$Height
#   # )
#   # PSPmeasure[, biomass := model$biomass]
#   
#   # Stand age
#   PSPplot[, stand_age := baseSA + (MeasureYear - baseYear)]
#   
#   # Height P90
#   PSPmeasure_cleanhgt <- PSPmeasure[!is.na(Height)]
#   height <- PSPmeasure_cleanhgt[, .(height = quantile(Height, probs = 0.90, na.rm = TRUE)),
#                                    by = .(OrigPlotID1, MeasureYear)]
#   
#   # Spatial join and buffer
#   PSPplot_info <- merge(st_transform(PSPgis, 5072), PSPplot, by = "OrigPlotID1")
#   PSPplot_info <- PSPplot_info[!is.na(PSPplot_info$PlotSize) & PSPplot_info$PlotSize > 0, ]
#   PSPplot_info$buffer_radius <- sqrt(PSPplot_info$PlotSize * 10000 / pi)
#   PSP_buf <- st_buffer(PSPplot_info, dist = PSPplot_info$buffer_radius)
#   
#   # Nearest SCANFI year match
#   years <- seq(1985, 2020, 5)
#   PSP_buf$scanfi_year <- sapply(PSP_buf$MeasureYear, function(y) years[which.min(abs(years - y))])
#   PSP_buf$layer_name <- paste0("y", PSP_buf$scanfi_year)
#   #browser()
#   # Load SCANFI height/closure
#   closure_stack <- rast(file.path(scanfi_dir, paste0("SCANFIclosure_5x5_", years, ".tif")))
#   height_stack <- rast(file.path(scanfi_dir, paste0("SCANFIheight_5x5_", years, ".tif")))
#   names(closure_stack) <- paste0("y", years)
#   names(height_stack)  <- paste0("y", years)
#   
#   # Extract values
#   val_closure <- terra::extract(closure_stack, vect(PSP_buf), fun = mean, na.rm = TRUE)
#   val_height  <- terra::extract(height_stack, vect(PSP_buf), fun = mean, na.rm = TRUE)
#   
#   val_closure$OrigPlotID1 <- PSP_buf$OrigPlotID1
#   val_height$OrigPlotID1 <- PSP_buf$OrigPlotID1
#  # browser()
#   val_closure <- melt(as.data.table(val_closure)[, !"ID"], id.vars = "OrigPlotID1",
#                       variable.name = "layer_name", value.name = "closure")
#   
#   val_height  <- melt(as.data.table(val_height)[, !"ID"], id.vars = "OrigPlotID1",
#                       variable.name = "layer_name", value.name = "height_SCANFI")
#   
#   # Summarize by mean
#   val_closure <- val_closure[, .(closure = base::mean(closure, na.rm = TRUE)), by = .(OrigPlotID1, layer_name)]
#   val_height  <- val_height[, .(height_SCANFI  = base::mean(height_SCANFI,  na.rm = TRUE)), by = .(OrigPlotID1, layer_name)]
#   
#   # Metadata merge
#   meta <- as.data.table(PSP_buf)[, .(OrigPlotID1, MeasureYear, stand_age, scanfi_year, layer_name)]
#   
#   scanfi_merged <- Reduce(function(x, y) merge(x, y, by = c("OrigPlotID1", "layer_name"), all = TRUE),
#                           list(meta, val_closure, val_height))
#   
#   scanfi_summary <- scanfi_merged[, .(
#     closure = mean(closure, na.rm = TRUE),
#     height_SCANFI  = mean(height_SCANFI,  na.rm = TRUE)
#   ), by = .(OrigPlotID1, MeasureYear)]
#   
#   # Biomass + stand age
#   psp_biomass <- PSPmeasure[, .(biomass = sum(biomass, na.rm = TRUE)),
#                             by = .(OrigPlotID1, MeasureYear)]
#   
#   psp_age <- PSPplot[, .(OrigPlotID1, MeasureYear, stand_age, Elevation, PlotSize)]
#   
#   # Broadleaf % calc
#   # PSPmeasure[, newSpeciesName_clean := tolower(newSpeciesName)]
#   # sppEquiv <- unique(LandR::sppEquivalencies_CA[, .(EN_generic_full_clean = tolower(EN_generic_full), Broadleaf)])
#   # PSPmeasure <- merge(PSPmeasure, sppEquiv, by.x = "newSpeciesName_clean", by.y = "EN_generic_full_clean", all.x = TRUE)
#   PSPmeasure[, is_broadleaf := Broadleaf == "TRUE"]
#   
#   biomass_summary <- PSPmeasure[, .(
#     total_biomass = sum(biomass, na.rm = TRUE),
#     broadleaf_biomass = sum(biomass[is_broadleaf], na.rm = TRUE)
#   ), by = .(OrigPlotID1, MeasureYear)]
#   
#   biomass_summary[, broadleaf_prop := broadleaf_biomass / total_biomass]
#   
#   # Final model data merge
#   model_data <- Reduce(function(x, y) merge(x, y, by = c("OrigPlotID1", "MeasureYear"), all = TRUE),
#                        list(scanfi_summary, psp_biomass, psp_age, height,
#                             biomass_summary[, .(OrigPlotID1, MeasureYear, broadleaf_prop)]))
#   
#   return(na.omit(model_data))
# }


prepareCanopyModelData <- function(
    scanfi_dir,
    psp_path,
    climate_path,
    ecoregion = NULL
) {
  
  library(data.table)
  library(sf)
  library(terra)
  library(LandR)
  library(pemisc)
  library(PSPclean)
  # download and prepare PSP data
  message("Preparing canopy model data (v2)")
  psp <- getPSP(
    PSPdataTypes = c("BC", "QC", "ON", "NB", "AB", "NFI", "SK"),
    destinationPath = psp_path
  )
  
  PSPmeasure <- as.data.table(psp$PSPmeasure)
  PSPplot    <- as.data.table(psp$PSPplot)
  PSPgis     <- st_as_sf(psp$PSPgis)
  
  ## calculate biomass
  sppEquiv <- unique(LandR::sppEquivalencies_CA, by = "Latin_full")
  
  # PSPmeasure <- merge(
  #   PSPmeasure,
  #   sppEquiv,
  #   by.x = "Species",
  #   by.y = "Latin_full",
  #   all.x = TRUE
  # )
  PSPmeasure <- merge(
    PSPmeasure,
    sppEquiv,
    by.x = "Species",
    by.y = "Latin_full",
    all.x = TRUE,
    suffixes = c("", "_spp")
  )
  #browser()
  PSPmeasure <- PSPmeasure[!is.na(PSP_spp)]
  
  PSPmeasure[, biomass := pemisc::biomassCalculation(
    species = PSP_spp,
    DBH = DBH,
    includeHeight = TRUE,
    height = Height
  )$biomass]
  
  psp_biomass <- PSPmeasure[
    , .(biomass = sum(biomass, na.rm = TRUE)),
    by = .(OrigPlotID1, MeasureYear)
  ]
  
  ## stand age
  ## ------------------------------------------------------------------
  PSPplot[, stand_age := baseSA + (MeasureYear - baseYear)]
  
  psp_age <- PSPplot[
    , .(OrigPlotID1, MeasureYear, stand_age, Elevation)
  ]
  
  ## Broadleaf_prop
  ## ------------------------------------------------------------------ 
  PSPmeasure[, is_broadleaf := Broadleaf == "TRUE"]
  # spp_col <- intersect(names(PSPmeasure), c("speciesCode","SpeciesCode","species","Species","spp","sppCode"))
  # if (length(spp_col) == 0) stop("PSPmeasure has no recognizable species column for broadleaf_spp extraction.")
  # spp_col <- spp_col[1]
  spp_col <-"LandR"
  browser()
  # broadleaf species list ( needed downstream for cohortData broadleaf_prop)
  broadleaf_spp <- unique(PSPmeasure[is_broadleaf == TRUE, get(spp_col)])
  broadleaf_spp <- broadleaf_spp[!is.na(broadleaf_spp)]
  
  broadleaf_summary <- PSPmeasure[
    , .(
      total_biomass = sum(biomass, na.rm = TRUE),
      broadleaf_biomass = sum(biomass[is_broadleaf], na.rm = TRUE)
    ),
    by = .(OrigPlotID1, MeasureYear)
  ]
  
  broadleaf_summary[
    , broadleaf_prop := broadleaf_biomass / total_biomass
  ]
  
  ## height P90, from PSP data
  ## ------------------------------------------------------------------ 
  PSPmeasure_clean <- PSPmeasure[!is.na(Height)]
  
  height_p90 <- PSPmeasure_clean[
    , .(height_p90 = quantile(Height, probs = 0.90, na.rm = TRUE)),
    by = .(OrigPlotID1, MeasureYear)
  ]
  
  ## HEight and closure from SCANFI rasters, from multiple years
  ## ------------------------------------------------------------------
  PSPplot_sf <- merge(PSPgis, PSPplot, by = "OrigPlotID1")
  PSPplot_sf <- st_transform(PSPplot_sf, 5072)
  
  
  ## get X and Y for spatial K mean clustering later
  coords <- st_coordinates(PSPplot_sf)
  PSPplot_sf$X <- coords[,1]
  PSPplot_sf$Y <- coords[,2]
  
  
  PSPplot_sf_dt <- as.data.table(PSPplot_sf)[
    , .(OrigPlotID1, MeasureYear, X, Y)
  ]
  ##
  PSPplot_sf$buffer_radius <-
    sqrt(PSPplot_sf$PlotSize * 10000 / pi)
  
  PSP_buf <- st_buffer(PSPplot_sf, PSPplot_sf$buffer_radius)
  
  years <- seq(1985, 2020, 5)
  
  ### CHange APril 2026 for strict match
  # keep only exact matches
  PSP_buf <- PSP_buf[PSP_buf$MeasureYear %in% years, ]
  
  # assign directly
  PSP_buf$scanfi_year <- PSP_buf$MeasureYear
  
  # PSP_buf$scanfi_year <- sapply(
  #   PSP_buf$MeasureYear,
  #   function(y) years[which.min(abs(years - y))]
  # )
  
  PSP_buf$layer_name <- paste0("y", PSP_buf$scanfi_year)
  
  closure_stack <- rast(
    file.path(scanfi_dir, paste0("SCANFIclosure_1km_", years, ".tif")) #SCANFIclosure_5x5_
  )
  
  height_stack <- rast(
    file.path(scanfi_dir, paste0("SCANFIheight_1km_", years, ".tif")) #SCANFIheight_5x5_
  )
  
  names(closure_stack) <- paste0("y", years)
  names(height_stack)  <- paste0("y", years)
  
  ## Extract closure
  val_closure <- terra::extract(
    closure_stack,
    vect(PSP_buf),
    fun = mean,
    na.rm = TRUE
  )
  val_closure$OrigPlotID1 <- PSP_buf$OrigPlotID1
  val_closure <- as.data.table(val_closure)
  
  val_closure_long <- melt(
    val_closure[, !"ID"],
    id.vars = "OrigPlotID1",
    variable.name = "layer_name",
    value.name = "closure"
  )
  
  val_closure_long <- val_closure_long[
    , .(closure = mean(closure, na.rm = TRUE)),
    by = .(OrigPlotID1, layer_name)
  ]
  
  ## Extract height
  val_height <- terra::extract(
    height_stack,
    vect(PSP_buf),
    fun = mean,
    na.rm = TRUE
  )
  val_height$OrigPlotID1 <- PSP_buf$OrigPlotID1
  val_height <- as.data.table(val_height)
  
  val_height_long <- melt(
    val_height[, !"ID"],
    id.vars = "OrigPlotID1",
    variable.name = "layer_name",
    value.name = "scanfi_height"
  )
  
  val_height_long <- val_height_long[
    , .(scanfi_height = mean(scanfi_height, na.rm = TRUE)),
    by = .(OrigPlotID1, layer_name)
  ]
  
  ## Metadata
  meta <- as.data.table(PSP_buf)[
    , .(OrigPlotID1, MeasureYear, layer_name)
  ]
  #browser()
  ## merge datasets
  scanfi_merged <- Reduce(
    function(x, y) merge(x, y, by = c("OrigPlotID1", "layer_name"), all = TRUE),
    list(meta, val_closure_long, val_height_long)
  )
  
  scanfi_summary <- scanfi_merged[
    , .(
      closure = mean(closure, na.rm = TRUE),
      scanfi_height = mean(scanfi_height, na.rm = TRUE)
    ),
    by = .(OrigPlotID1, MeasureYear)
  ]
  
  ## climate data 
  ## ------------------------------------------------------------------
  browser()
  PSP_climate <- fread(climate_path)
  
  PSP_climate_v1 <- PSP_climate[
    , `:=`(
      Year = as.integer(Year),
      id1  = as.character(id1),
      id2  = as.character(id2)
    )
  ]
  
  # convert everything else to numeric
  num_cols <- setdiff(
    names(PSP_climate_v1),
    c("Year", "id1", "id2", "Latitude", "Longitude")
  )
  
  PSP_climate_v1[
    , (num_cols) := lapply(.SD, as.numeric),
    .SDcols = num_cols
  ]
  
  # standardize join keys
  setnames(PSP_climate_v1, "id1", "OrigPlotID1")
  
  ## keep spatial column for spatial clustering, if needed
  
  # drop spatial columns (not predictors)
  PSP_climate_v1 <- PSP_climate_v1[
    , !c("Latitude", "Longitude"), with = FALSE
  ]
  ## Get rid of Rad, seems like Raddoesnt have data after 2010 (i.e. Rad01), 269 VS ?
  PSP_climate_v1 <- PSP_climate_v1[, .SD, .SDcols = !grepl("Rad", names(PSP_climate_v1))]
  
  ## ecodistrict (grouping only)
  ## ------------------------------------------------------------------
  # if (!is.null(ecoregion_path)) {
  #   eco <- st_read(ecoregion_path)
  #   eco <- st_transform(eco, 5072)
  if (!is.null(ecoregion)) {
    eco <- ecoregion
    eco <- st_transform(eco, 5072)
    
    intersected <- st_intersection(PSP_buf, eco)
    intersected$area <- st_area(intersected)
    
    eco_dt <- as.data.table(intersected)[
      , .SD[which.max(area)],
      by = OrigPlotID1
    ][
      , .(OrigPlotID1, ECOPROVINC) #ECOREGION, ECOPROVINC, ECODISTRIC for ecodistrict, change name for whichever level is used
    ]
  } else {
    eco_dt <- NULL
  }
  
  ## final merge
  ## ------------------------------------------------------------------
  #browser()
  model_data <- Reduce(
    function(x, y) merge(x, y, by = c("OrigPlotID1", "MeasureYear"), all = TRUE),
    list(
      scanfi_summary,
      height_p90,
      psp_biomass,
      psp_age,
      PSPplot_sf_dt,
      broadleaf_summary[, .(OrigPlotID1, MeasureYear, broadleaf_prop)]
      #,PSP_climate_v1   # <-- climate table after renaming & cleaning
    )
  )
  model_data <- merge(
    model_data,
    PSP_climate_v1,
    by.x = c("OrigPlotID1", "MeasureYear"),
    by.y = c("OrigPlotID1", "Year"),
    all.x = TRUE
  )
  if (!is.null(eco_dt)) {
    model_data <- merge(
      model_data,
      eco_dt,
      by = "OrigPlotID1",
      all.x = TRUE
    )
    if ("ECOPROVINC" %in% names(model_data)) { #ECOPROVINC
      model_data[, ECOPROVINC := as.factor(ECOPROVINC)]
    }
  }
  
  #model_data <- na.omit(model_data)
  model_data <- model_data[
    !is.na(height_p90) &
      !is.na(closure)
  ]
  
  # trying to see if interactive and log transformed variable helps to capture the noise in young stand or systematic bias in old stands 
  
  # model_data[, age2 := stand_age^2]
  # model_data[, log_age := log(stand_age + 1)]
  # model_data[, biomass_age := biomass / (stand_age + 1)]
  # model_data[, age_biomass := stand_age * biomass]
  # 
  
  attr(model_data, "broadleaf_spp") <- broadleaf_spp
  message("Finished canopy model data (v2)")
  return(model_data)
}
