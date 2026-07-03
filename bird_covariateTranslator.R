## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "bird_covariateTranslator",
  description = "Translates LandR cohort outputs into structure rasters (height, closure, biomass) 
                  using PSP-trained GPBoost/XGBoost models, and stores them as dynamic covariates.",
  keywords = c("dynamic covariates", "bird modeling"),
  authors = structure(list(list(given = c("Sourav", ""), family = "Das", role = c("aut", "cre"), email = "souravdron@gmail.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(bird_covariateTranslator = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "bird_covariateTranslator.Rmd"),
  reqdPkgs = list( "ggplot2","data.table", "terra", "sf",  "httr2", "dplyr", "RCurl", "XML", "snow", "googledrive",
                  "PSPclean", "pemisc","ranger", "caret", "xgboost", "SHAPforxgboost", #"PredictiveEcology/reproducible@AI (HEAD)", "PredictiveEcology/SpaDES.core@box (>= 2.1.5.9022)",
                  "PredictiveEcology/SpaDES.core@box"),
  parameters = bindrows(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                          "area obtained using `reproducible::studyAreaName()`"),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?"),
    defineParameter("predictStartYear", "numeric", NA, NA, NA,
                    desc = "First year to predict canopy and height"),
    defineParameter("predictEndYear", "numeric", NA, NA, NA,
                    desc = "Last year to predict canopy and height"),
    defineParameter("predictInterval", "numeric", 1, NA, NA,
                    desc = "Interval between years to predict"),
    defineParameter("cohortFolder", "character", "outputs/", 
                    desc = "Path to cohort rasters"),
    defineParameter("pixelGroupFolder", "character", "outputs/", 
                    desc = "Path to pixelGroup rasters"),
    defineParameter("scanfiDir",   "character", NA, NA, NA,
                    desc = "Path to SCANFI 5x5 dir or base folder (optional; downloader can be used inside prepareCanopyModelData)"),
    defineParameter("pspDataPath", "character", NA, NA, NA,
                    desc = "Optional path to preprocessed PSP (RDS/CSV). If not provided, PSP will be downloaded & processed."),
    defineParameter("pspClimatePath", "character", NA, NA, NA,
                    desc = "Path to PSP climate CSV used for canopy model training"),
    defineParameter("ecoregionURL", "character", NA, NA, NA,
                    desc = "URL or path to ecoprovince/ecoregion vector"),
    defineParameter("covariateSourceMode", "character", "dynamic", NA, NA,
                    desc = "Which covariate source to prefer when both observed and translator/modelled versions exist. Options: 'observed', 'dynamic', or 'hybrid'",
                    "default is dynamic")
  ),
  inputObjects = bindrows(
    expectsInput("studyAreaRas", "SpatRaster", 
                 desc = paste("Study area raster to match output to")),
    expectsInput("pspData", "data.table", 
                 desc = paste("Preprocessed PSP data with stand_age, biomass, vegetation height, canopy closure etc.")),
    expectsInput("cohortData", "data.table", 
                 desc = paste("`data.table` with cohort-level information on age and biomass, by `pixelGroup` and ecolocation",
                              "(i.e., `ecoregionGroup`). If supplied, it must have the following columns: `pixelGroup` (integer),",
                              "`ecoregionGroup` (factor), `speciesCode` (factor), `B` (integer in $g/m^2$), `age` (integer in years)")),
    expectsInput("pixelGroupMap", "SpatRaster", 
                 desc = paste("A raster layer with `pixelGroup` IDs per pixel. Pixels are grouped" ,
                              "based on identical `ecoregionGroup`, `speciesCode`, `age` and `B` composition,",
                              "even if the user supplies other initial groupings (e.g., via the `Biomass_borealDataPrep`",
                              "module."),
    expectsInput("varmetaTable", "data.frame",
                 desc = "Table with metadata for each variable: name, type, dynamic/static, lag, etc."),
    ## NEW ADDITION
    expectsInput("climateYearRecord", "data.table",
                 desc = "Optional table with simYear and climate_year mapping"),
    expectsInput("ecoregionVector", "sf",
                 desc = "Ecoprovince/ecoregion vector used for assigning ECOPROVINC in canopy model training")
    )
    
  ),
  outputObjects = bindrows(
    createsOutput("dynamic", "list", 
                  desc = "List of dynamically created rasters by year"),
    createsOutput("varmetaTable", "data.frame", 
                  desc = "Updated variable metadata table"),
    createsOutput("height_model", "xgb.Booster", 
                  desc = "Trained model for height prediction"),
    createsOutput("closure_model", "xgb.Booster", 
                  desc = "Trained model for closure prediction")
  )
))
#browser()
doEvent.bird_covariateTranslator <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    "init" = {
      sim <- InitTranslator(sim)
      #sim <- scheduleEvent(sim, time(sim), "bird_covariateTranslator", "predict")
      sim <- scheduleEvent(sim,P(sim)$predictStartYear,"bird_covariateTranslator","buildCovariates", eventPriority = 6)
    },

    "buildCovariates" = {
      sim <- PredictTranslator(sim)
      nextYear <- time(sim) + P(sim)$predictInterval
      if (nextYear <= P(sim)$predictEndYear) {
        sim <- scheduleEvent(sim, nextYear,"bird_covariateTranslator", "buildCovariates", eventPriority = 6)
      }
    },
    
    # ## new correction
    # "buildCovariates" = {
    #   yr <- time(sim)  # Explicit current year
    #   message(crayon::blue(" bird_covariateTranslator: Building dynamic covariates for year: ", yr))
    #   
    #   sim <- PredictTranslator(sim)
    #   
    #   # Don't schedule again if start == end
    #   if (P(sim)$predictStartYear != P(sim)$predictEndYear) {
    #     nextYear <- yr + P(sim)$predictInterval
    #     if (nextYear <= P(sim)$predictEndYear) {
    #       sim <- scheduleEvent(sim, nextYear, "bird_covariateTranslator", "buildCovariates")
    #     }
    #   }
    # },
    
    # "save" = {
    #   sim <- SaveTranslator(sim)
    # }
  )
  return(invisible(sim))
}



InitTranslator <- function(sim) {
  message("Initializing bird_covariateTranslator...")
  
# Load cohort + pixelGroup (from sim object or folder)
browser()
  if (is.null(sim$cohortData)) {
    message(bold(cyan("Loading cohortData from folder: ")), P(sim)$cohortFolder) ##file.path(outputPath(sim))
    #cohort_path <- list.files(P(sim)$cohortFolder, pattern = "cohortData.*rds$", full.names = TRUE)
    cohort_path <- list.files(P(sim)$cohortFolder, pattern = "cohortData.*rds$", full.names = TRUE) #file.path(outputPath(sim)
    if (length(cohort_path) == 0) stop("No cohortData file found in ", P(sim)$cohortFolder)
    #sim$cohortData <- readRDS(cohort_path[1])
    #sim$cohortData <- loadYearMatchedFile(P(sim)$cohortFolder, "cohortData.*rds$", time(sim), readRDS)
    sim$cohortData <- loadYearMatchedFile(P(sim)$cohortFolder, "cohortData.*rds$", time(sim), readRDS)
  }
  
  if (is.null(sim$pixelGroupMap)) {
    message(bold(cyan("Loading pixelGroupMap from folder: ")), P(sim)$cohortFolder)
    #pg_path <- list.files(P(sim)$pixelGroupFolder, pattern = "pixelGroup.*tif$", full.names = TRUE)
    pg_path <- list.files(P(sim)$cohortFolder, pattern = "pixelGroup.*tif$", full.names = TRUE)
    if (length(pg_path) == 0) stop("No pixelGroupMap raster found in ", P(sim)$cohortFolder)
    #sim$pixelGroupMap <- terra::rast(pg_path[1])
    #sim$pixelGroupMap <- loadYearMatchedFile(P(sim)$pixelGroupFolder, "pixelGroupMap.*tif$", time(sim), terra::rast)
    sim$pixelGroupMap <- loadYearMatchedFile(P(sim)$cohortFolder, "pixelGroupMap.*tif$", time(sim), terra::rast)
  }
  
# prepare PSP & SCANFI data
#browser()
  # if (!suppliedElsewhere("pspData", sim)) {
  #   message("Preparing PSP and SCANFI data...")
  #   # sim$pspData <- Cache(prepareCanopyModelData,
  #   #                      scanfi_dir = P(sim)$scanfi_dir,
  #   #                      psp_path   = P(sim)$psp_path)
  #   sim$pspData <- prepareCanopyModelData(scanfi_dir = P(sim)$scanfi_dir,
  #                                   climate_path = P(sim)$psp_path,
  #                                   ecoregion_path = "inputs/Ecoprovinces/ecoprovinces.shp")
  # }
  # 
  # 
  # # Train canopy models
  # message("Training canopy height & closure models...")
  # trained <- Cache(trainCanopyheightModels,
  #                  pspData = sim$pspData,
  #                  useCV   = FALSE) #nFolds = 1
  # sim$height_model  <- trained$height_model
  # sim$closure_model <- trained$closure_model

  ## NEW ADDITION APRIL 2026
  message(bold(cyan(("Preparing canopy model training data with updated XGBoost workflow..."))))
  
  sim$canopyModelData <- Cache(
    prepareCanopyModelData,
    scanfi_dir    = P(sim)$scanfiDir,
    psp_path      = P(sim)$pspDataPath,
    climate_path  = P(sim)$pspClimatePath,
    ecoregion     = sim$ecoregionVector
  )
  
  sim$broadleaf_spp <- attr(sim$canopyModelData, "broadleaf_spp")
  
  ## Canopy model training settings.
  ## Change these manually if RF/XGBoost tuning strategy changes.
  nClimatePredictors <- 10
  corrCutoff <- 0.8
  canopyXFolds <- 5
  
  message("Selecting height predictors using RF...")
  height_feat <- Cache(
    selectCanopyPredictors_RF,
    model_data  = sim$canopyModelData,
    response    = "height_p90",
    n_vars      = nClimatePredictors,
    corr_cutoff = corrCutoff
  )
  
  message("Selecting closure predictors using RF...")
  closure_feat <- Cache(
    selectCanopyPredictors_RF,
    model_data  = sim$canopyModelData,
    response    = "closure",
    n_vars      = nClimatePredictors,
    corr_cutoff = corrCutoff
  )
  
  message("Training XGBoost height model...")
  sim$height_xgb <- Cache(
    trainCanopyXGB,
    model_data = sim$canopyModelData,
    response   = "height_p90",
    predictors = height_feat$predictors,
    nFolds     = canopyXFolds,
    figDir     = file.path(outputPath(sim), "figs", "xgb_height")
  )
  
  message("Training XGBoost closure model...")
  sim$closure_xgb <- Cache(
    trainCanopyXGB,
    model_data = sim$canopyModelData,
    response   = "closure",
    predictors = closure_feat$predictors,
    nFolds     = canopyXFolds,
    figDir     = file.path(outputPath(sim), "figs", "xgb_closure")
  )
  
  sim$canopyModels <- list(
    height  = sim$height_xgb,
    closure = sim$closure_xgb
  )

  ## Correction 13th may
  
#   # Initialize dynamic + meta table
#   if (is.null(sim$dynamic)) sim$dynamic <- list()
#   if (is.null(sim$varmetaTable)) sim$varmetaTable <- data.frame()
#   
#   return(invisible(sim))
# }
  # Initialize dynamic + metadata objects
  if (is.null(sim$dynamic)) sim$dynamic <- list()
  if (is.null(sim$varmetaTable)) sim$varmetaTable <- data.frame()
  
  # Preserve original bird_dataPrep metadata once.
  # Translator will rebuild sim$varmetaTable from this base + current dynamic metadata.
  if (is.null(sim$varmetaTable_base)) {
    sim$varmetaTable_base <- sim$varmetaTable
  }
  
  sim$varmetaTable_dynamic <- data.frame()
  
  return(invisible(sim))
}

#  Prediction Event (create dynamic rasters)

PredictTranslator <- function(sim) {
  # Extract prediction range from parameters
  predictStartYear <- P(sim)$predictStartYear
  predictEndYear   <- P(sim)$predictEndYear
  predictInterval  <- P(sim)$predictInterval
  
  # Safety checks within simulation range 
  if (predictStartYear < start(sim)) {
    warning(sprintf(
      "predictStartYear (%s) is before simulation start (%s). Adjusting.",
      predictStartYear, start(sim)
    ))
    predictStartYear <- start(sim)
  }
  
  if (predictEndYear > end(sim)) {
    warning(sprintf(
      "predictEndYear (%s) exceeds simulation end (%s). Adjusting.",
      predictEndYear, end(sim)
    ))
    predictEndYear <- end(sim)
  }
  
  if (is.na(predictStartYear) || is.na(predictEndYear)) {
    stop("Please set predictStartYear and predictEndYear in setupProject() call.")
  }
  
  # years <- seq(predictStartYear, predictEndYear, by = predictInterval)
  # message(sprintf("Final prediction range: %s–%s (interval %s)", 
  #                 predictStartYear, predictEndYear, predictInterval))
  yr <- time(sim)
  #message("Predicting for year: ", yr)
  #browser()
  
  ## NEw ADD READ COHORT AND PIXEL GROUP MAP dynamically, may be not needed for pixel group map?
  # sim$cohortData <- loadYearMatchedFile(
  #   file.path(outputPath(sim)),
  #   "cohortData.*rds$",
  #   yr,
  #   readRDS
  # )
  # 
  # sim$pixelGroupMap <- loadYearMatchedFile(
  #   file.path(outputPath(sim)),
  #   "pixelGroupMap.*tif$",
  #   yr,
  #   terra::rast
  # )
  
  ## NEW ADD 2 Start
  #yr <- time(sim)
  
  if (is.null(sim$cohortData)) {
    message("Translator: cohortData not found in simlist — loading from disk")
    sim$cohortData <- loadYearMatchedFile(
      file.path(outputPath(sim)),
      "cohortData.*rds$",
      yr,
      readRDS
    )
  } else {
    message("Translator: using cohortData supplied by LandR for year ", yr)
  }
  
  if (is.null(sim$pixelGroupMap)) {
    message("Translator: pixelGroupMap not found in sim — loading from disk")
    sim$pixelGroupMap <- loadYearMatchedFile(
      file.path(outputPath(sim)),
      "pixelGroupMap.*tif$",
      yr,
      terra::rast
    )
  } else {
    message("Translator: using pixelGroupMap supplied by LandR")
  }
  
  sim$ecoprovinceRaster <- makeEcoprovinceRaster(
    ecoregionVector = sim$ecoregionVector,
    template        = sim$pixelGroupMap,
    field           = "ECOPROVINC"
  )
  
  ## NEW ADD 2 END
  
  #  Predict canopy structure by year
  # for (yr in years) {
    message(bold$green(" Building dynamic covariates for year: "), bold$yellow(yr))
    # preds <- predictClosureHeightFromCohort(
    #   cohortData    = sim$cohortData,
    #   pixelGroupMap = sim$pixelGroupMap,
    #   height_model  = sim$height_model,
    #   closure_model = sim$closure_model,
    #   studyAreaRas  = sim$studyAreaRas,
    #   year          = yr
    # )
    
## NEW ADDITION APRIL 2026
  canopy_predictors <- unique(unlist(
    lapply(sim$canopyModels, function(x) x$predictors)
  ))
  
  # canopy_climate_year <- yr
  # 
  # if (!is.null(sim$climateYear)) {
  #   canopy_climate_year <- sim$climateYear
  # }
  
  ## nEW addition for prediction using climateYear
  browser()
  canopy_climate_year <- yr
  
  if (!is.null(sim$climateYearRecord)) {
    climate_row <- sim$climateYearRecord[
      sim$climateYearRecord$simYear == yr, 
    ]
    
    if (nrow(climate_row) == 0) {
      stop("No climate year found in climateYearRecord for simulation year: ", yr)
    }
    
    canopy_climate_year <- climate_row$climateYear[1]
    
  } else if (!is.null(sim$climateYear)) {
    canopy_climate_year <- sim$climateYear
  }
  ## new addition ends
  ## correction 13th May
  # ## NEW ADD FOR UPDATING METADATA WITH CLIMATE LAYERS
  # 
  # ## Trying with the dynamic funciton
  # sim$varmetaTable <- dplyr::bind_rows(
  #   sim$varmetaTable,
  #   extractAvailableVariables_dynamic(sim)
  # ) |>
  #   dplyr::distinct()
  # ## NEW ADD ENDS
  # 
  # browser()
  # # climate_stack <- buildCanopyClimateStack(
  # #   sim = sim,
  # #   year = canopy_climate_year,
  # #   canopy_predictors = canopy_predictors
  # # )
  ## Rebuild metadata from stable bird_dataPrep base + current translator/canClimate inventory.
  ## Do not repeatedly append to sim$varmetaTable across simulation years.
  if (is.null(sim$varmetaTable_base)) {
    sim$varmetaTable_base <- sim$varmetaTable
  }
  
  sim$varmetaTable_dynamic <- extractAvailableVariables_dynamic(sim)
  
  sim$varmetaTable <- dplyr::bind_rows(
    sim$varmetaTable_base,
    sim$varmetaTable_dynamic
  ) |>
    dplyr::distinct(source, full, source_var, year, bird_label, canopy_label, .keep_all = TRUE)
  ## NEW ADD ENDS
  
  ### NEW ADDITION FOR CLIMATE DATA
  climate_stack <- buildCanopyClimateStack(
    sim = sim,
    year = canopy_climate_year,
    canopy_predictors = canopy_predictors,
    varmetaTable = sim$varmetaTable
  )
  ## NEW ADDITION ENDS
  preds <- predictCanopyFromCohort(
    cohortData         = sim$cohortData,
    pixelGroupMap      = sim$pixelGroupMap,
    studyAreaRas       = sim$studyAreaRas,
    climate_stack      = climate_stack,
    ecoprovince_raster = sim$ecoprovinceRaster,
    canopyModels       = sim$canopyModels,
    broadleaf_species  = sim$broadleaf_spp
  )
  ## new addition ends
  
    # Biomass raster
    biomass_1km <- makeBiomassRaster(sim$cohortData, sim$pixelGroupMap)
    biomass_1km <- terra::project(biomass_1km, sim$studyAreaRas)
    #browser()
    
    
    # renames height, closure and biomass layer
    # names(preds$height_1km)  <- paste0("SCANFIheight_1km_", yr)
    # names(preds$closure_1km) <- paste0("SCANFIclosure_1km_", yr)
    # names(biomass_1km)       <- paste0("SCANFIbiomass_1km_", yr)
    # # Store outputs in dynamic list
    # sim$dynamic[[paste0("SCANFIheight_1km_", yr)]]  <- preds$height_1km
    # sim$dynamic[[paste0("SCANFIclosure_1km_", yr)]] <- preds$closure_1km
    # sim$dynamic[[paste0("SCANFIbiomass_1km_", yr)]] <- biomass_1km
    
    # height_ras  <- preds$SCANFIheight_1km
    # closure_ras <- preds$SCANFIclosure_1km
    height_ras  <- preds$height
    closure_ras <- preds$closure
    # enforce naming (safe)
    names(height_ras)  <- paste0("SCANFIheight_1km_", yr)
    names(closure_ras) <- paste0("SCANFIclosure_1km_", yr)
    
    # biomass naming also required
    names(biomass_1km) <- paste0("SCANFIbiomass_1km_", yr)
    
    # store
    sim$dynamic[[paste0("SCANFIheight_1km_", yr)]]  <- height_ras
    sim$dynamic[[paste0("SCANFIclosure_1km_", yr)]] <- closure_ras
    sim$dynamic[[paste0("SCANFIbiomass_1km_", yr)]] <- biomass_1km
    # Update varMetaTable
    # new_meta <- data.frame(
    #   varName = c(paste0("SCANFIheight_1km_", yr),
    #               paste0("SCANFIclosure_1km_", yr),
    #               paste0("SCANFIbiomass_1km_", yr)),
    #   type = c("height", "closure", "biomass"),
    #   source = "translator",
    #   year = yr,
    #   dynamic = TRUE,
    #   stringsAsFactors = FALSE
    # )
    ## correction 13th may
    # sim$vars_available_dynamic <- extractAvailableVariables_dynamic(sim)
    # sim$varmetaTable <- dplyr::bind_rows(
    #   sim$varmetaTable,
    #   sim$vars_available_dynamic
    # ) |> dplyr::distinct()
    ## Rebuild metadata again after new dynamic rasters are added.
    ## This makes SCANFIheight/closure/biomass for the current simulation year visible.
    sim$varmetaTable_dynamic <- extractAvailableVariables_dynamic(sim)
    
    sim$varmetaTable <- dplyr::bind_rows(
      sim$varmetaTable_base,
      sim$varmetaTable_dynamic
    ) |>
      dplyr::distinct(source, full, source_var, year, bird_label, canopy_label, .keep_all = TRUE)
    
  #}

  #  Rewrite metadata?
  # if (exists("buildRasterStackAllYears")) {
  #   message("Rebuilding variable metadata with buildRasterStackAllYears()...")
  #   sim$varMetaTable <- buildRasterStackAllYears(
  #     outSim = sim,
  #     lag_df = data.frame(),  # placeholder if not needed
  #     vars_available = sim$varMetaTable,
  #     years = years
  #   )
  # }
  # existing_years <- as.integer(names(sim$stack_list))
  # all_prediction_years <- years
  # 
  # missing_years <- setdiff(all_prediction_years, existing_years)
  # 
  # if (length(missing_years) > 0) {
  #   message("Building raster stacks for years: ", paste(missing_years, collapse=", "))
  #   browser()
  #   new_stacks <- buildRasterStackAllYears(
  #     outSim         = sim,
  #     lag_df         = sim$lag_df,
  #     vars_available = sim$varmetaTable,
  #     predictStartYear = min(missing_years),
  #     predictEndYear   = max(missing_years),
  #     predictInterval  = unique(diff(missing_years))[1]
  #   )
  #   
  #   # append to existing stack_list
  #   #browser()
  #   sim$stack_list <- c(sim$stack_list, new_stacks)
  # }
    # sim$stack_list[[as.character(yr)]] <- 
    #   buildRasterStackAllYears(
    #     outSim           = sim,
    #     lag_df           = sim$lag_df,
    #     vars_available   = sim$varmetaTable,
    #     predictStartYear = yr,
    #     predictEndYear   = yr,
    #     predictInterval  = P(sim)$predictInterval
    #   )[[as.character(yr)]]
    
    ### NEW SINGLE YEAR FUNCTION
    ## ALSO NEW ADDITION FOR FIRESENSE
    
    #browser()
    if (!is.null(sim$climateYearRecord)) {
      rec <- sim$climateYearRecord
      val <- rec$climateYear[rec$simYear == yr]
      if (length(val) != 1) {
        stop("climateYearRecord invalid or missing for simYear = ", yr)
      }
      climate_year <- val
    } else if (!is.null(sim$climateYear)) {
      climate_year <- sim$climateYear
    } else {
      climate_year <- yr
    }
    
    if (is.null(sim$stack_list)) sim$stack_list <- list()
    sim$stack_list[[as.character(yr)]] <- buildRasterStackAnnual(
      outSim         = sim,
      lag_df         = sim$lag_df,
      vars_available = sim$varmetaTable,
      prediction_year = yr,
      climate_year = climate_year
    )
    ## new addition 2026
    # Ensure bird_modelPredict runs immediately after translator finishes.
    # This is required because predict may have exited early due to missing covariates.
    # if ("bird_modelPredict" %in% modules(sim)) {
    #   sim <- scheduleEvent(sim, time(sim), "bird_modelPredict", "predict", eventPriority = 7)
    # }
    # Translator → Predictor handoff (only when translator created stacks)
    # if ("bird_modelPredict" %in% modules(sim) &&
    #     !is.null(sim$stack_list[[as.character(time(sim))]]) &&
    #     is.null(sim$predictedList[[as.character(time(sim))]])) {
    #   
    #   sim <- scheduleEvent(sim, time(sim),
    #                        "bird_modelPredict", "predict",
    #                        eventPriority = 7)
    # }
    
  # if (exists("stack_list", sim)) {
  #   message("Scheduling bird_modelPredict$predict after translator finishes…")
  #   sim <- scheduleEvent(sim, time(sim) + 1, "bird_modelPredict", "predict")
  # }
  return(invisible(sim))
}

#  Save Event




### template initialization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}
### template for save events
# Save <- function(sim) {
#   # ! ----- EDIT BELOW ----- ! #
#   # do stuff for this event
#   # sim <- saveFiles(sim)
# 
#   # ! ----- STOP EDITING ----- ! #
#   return(invisible(sim))
# }

### template for plot events
plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sampleData <- data.frame("TheSample" = sample(1:10, replace = TRUE))
  Plots(sampleData, fn = ggplotFn) # needs ggplot2

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
Event1 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event1Test1 <- " this is test for event 1. " # for dummy unit test
  # sim$event1Test2 <- 999 # for dummy unit test

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
Event2 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event2Test1 <- " this is test for event 2. " # for dummy unit test
  # sim$event2Test2 <- 777  # for dummy unit test

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$map <- Cache(prepInputs, extractURL('map')) # download, extract, load file from url in sourceURL
  # }

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #
  if (!suppliedElsewhere("ecoregionVector", sim)) {
    sim$ecoregionVector <- Cache(
      prepInputs,
      url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/province/ecoprovince_shp.zip", #P(sim)$ecoregionURL,
      destinationPath = dPath,
      fun = "sf::st_read",
      userTags = c("object:ecoregionVector")
    )
  }
  
  if (!suppliedElsewhere("climateYearRecord", sim)) {
    sim$climateYearRecord <- readRDS("~/SpaDES_book/LandRDemo_coreVeg/future/ian/ClimateYearTable_6_1_1.rds") ## hardcoded ClimateYearTable, ideally this would come from the simlist
  } 
  

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

ggplotFn <- function(data, ...) {
  ggplot2::ggplot(data, ggplot2::aes(TheSample)) +
    ggplot2::geom_histogram(...)
}

