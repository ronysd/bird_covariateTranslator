## This function downloads the PSP data, uses PSP data to match the closest SCANFI year (for closure and height) and extract
## height and closure data for the PSP locations, using a buffer around the PSP based on the plot size 


prepareCanopyModelData <- function(scanfi_dir, psp_path) {

  # Download and load PSP data
  psp_data <- getPSP(PSPdataTypes = c("BC", "QC", "ON", "NB", "AB", "NFI", "SK"), destinationPath = psp_path)
  PSPmeasure <- as.data.table(psp_data$PSPmeasure)
  PSPplot <- as.data.table(psp_data$PSPplot)
  PSPgis <- st_as_sf(psp_data$PSPgis)
  #browser()
  # Biomass calculation
  model <- biomassCalculation(
    species = PSPmeasure$newSpeciesName,
    DBH = PSPmeasure$DBH,
    includeHeight = TRUE,
    height = PSPmeasure$Height
  )
  PSPmeasure[, biomass := model$biomass]
  
  # Stand age
  PSPplot[, stand_age := baseSA + (MeasureYear - baseYear)]
  
  # Height P90
  PSPmeasure_cleanhgt <- PSPmeasure[!is.na(Height)]
  height <- PSPmeasure_cleanhgt[, .(height = quantile(Height, probs = 0.90, na.rm = TRUE)),
                                   by = .(OrigPlotID1, MeasureYear)]
  
  # Spatial join and buffer
  PSPplot_info <- merge(st_transform(PSPgis, 5072), PSPplot, by = "OrigPlotID1")
  PSPplot_info <- PSPplot_info[!is.na(PSPplot_info$PlotSize) & PSPplot_info$PlotSize > 0, ]
  PSPplot_info$buffer_radius <- sqrt(PSPplot_info$PlotSize * 10000 / pi)
  PSP_buf <- st_buffer(PSPplot_info, dist = PSPplot_info$buffer_radius)
  
  # Nearest SCANFI year match
  years <- seq(1985, 2020, 5)
  PSP_buf$scanfi_year <- sapply(PSP_buf$MeasureYear, function(y) years[which.min(abs(years - y))])
  PSP_buf$layer_name <- paste0("y", PSP_buf$scanfi_year)
  #browser()
  # Load SCANFI height/closure
  closure_stack <- rast(file.path(scanfi_dir, paste0("SCANFIclosure_5x5_", years, ".tif")))
  height_stack <- rast(file.path(scanfi_dir, paste0("SCANFIheight_5x5_", years, ".tif")))
  names(closure_stack) <- paste0("y", years)
  names(height_stack)  <- paste0("y", years)
  
  # Extract values
  val_closure <- extract(closure_stack, vect(PSP_buf), fun = mean, na.rm = TRUE)
  val_height  <- extract(height_stack, vect(PSP_buf), fun = mean, na.rm = TRUE)
  
  val_closure$OrigPlotID1 <- PSP_buf$OrigPlotID1
  val_height$OrigPlotID1 <- PSP_buf$OrigPlotID1
  #browser()
  val_closure <- melt(as.data.table(val_closure)[, !"ID"], id.vars = "OrigPlotID1",
                      variable.name = "layer_name", value.name = "closure_SCANFI")
  
  val_height  <- melt(as.data.table(val_height)[, !"ID"], id.vars = "OrigPlotID1",
                      variable.name = "layer_name", value.name = "height_SCANFI")
  
  # Summarize by mean
  val_closure <- val_closure[, .(closure_SCANFI = mean(closure_SCANFI, na.rm = TRUE)), by = .(OrigPlotID1, layer_name)]
  val_height  <- val_height[, .(height_SCANFI  = mean(height_SCANFI,  na.rm = TRUE)), by = .(OrigPlotID1, layer_name)]
  
  # Metadata merge
  meta <- as.data.table(PSP_buf)[, .(OrigPlotID1, MeasureYear, stand_age, scanfi_year, layer_name)]
  
  scanfi_merged <- Reduce(function(x, y) merge(x, y, by = c("OrigPlotID1", "layer_name"), all = TRUE),
                          list(meta, val_closure, val_height))
  
  scanfi_summary <- scanfi_merged[, .(
    closure_SCANFI = mean(closure_SCANFI, na.rm = TRUE),
    height_SCANFI  = mean(height_SCANFI,  na.rm = TRUE)
  ), by = .(OrigPlotID1, MeasureYear)]
  
  # Biomass + stand age
  psp_biomass <- PSPmeasure[, .(biomass = sum(biomass, na.rm = TRUE)),
                            by = .(OrigPlotID1, MeasureYear)]
  
  psp_age <- PSPplot[, .(OrigPlotID1, MeasureYear, stand_age, Elevation, PlotSize)]
  
  # Broadleaf % calc
  PSPmeasure[, newSpeciesName_clean := tolower(newSpeciesName)]
  sppEquiv <- unique(LandR::sppEquivalencies_CA[, .(EN_generic_full_clean = tolower(EN_generic_full), Broadleaf)])
  PSPmeasure <- merge(PSPmeasure, sppEquiv, by.x = "newSpeciesName_clean", by.y = "EN_generic_full_clean", all.x = TRUE)
  PSPmeasure[, is_broadleaf := Broadleaf == "TRUE"]
  
  biomass_summary <- PSPmeasure[, .(
    total_biomass = sum(biomass, na.rm = TRUE),
    broadleaf_biomass = sum(biomass[is_broadleaf], na.rm = TRUE)
  ), by = .(OrigPlotID1, MeasureYear)]
  
  biomass_summary[, broadleaf_prop := broadleaf_biomass / total_biomass]
  
  # Final model data merge
  model_data <- Reduce(function(x, y) merge(x, y, by = c("OrigPlotID1", "MeasureYear"), all = TRUE),
                       list(scanfi_summary, psp_biomass, psp_age, height,
                            biomass_summary[, .(OrigPlotID1, MeasureYear, broadleaf_prop)]))
  
  return(na.omit(model_data))
}
