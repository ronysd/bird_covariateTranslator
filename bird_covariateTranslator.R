## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "bird_covariateTranslator",
  description = "",
  keywords = "",
  authors = structure(list(list(given = c("Sourav", ""), family = "Das", role = c("aut", "cre"), email = "souravdron@gmail.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(bird_covariateTranslator = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "bird_covariateTranslator.Rmd"),
  reqdPkgs = list("PredictiveEcology/SpaDES.core@box (>= 2.1.5.9022)", "ggplot2",
                  "data.table", "terra", "sf",  "httr2", "dplyr", "RCurl", "XML", "snow", "googledrive",
                  "PredictiveEcology/reproducible@AI (HEAD)", "PSPclean", "pemisc",
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
                    desc = "First year to predict canopy structure"),
    defineParameter("predictEndYear", "numeric", NA, NA, NA,
                    desc = "Last year to predict canopy structure"),
    defineParameter("predictInterval", "numeric", 1, NA, NA,
                    desc = "Interval between years to predict"),
    defineParameter("cohortFolder", "character", "outputs/", 
                    desc = "Path to cohort rasters"),
    defineParameter("pixelGroupFolder", "character", "outputs/", 
                    desc = "Path to pixelGroup rasters"),
    defineParameter("scanfiDir",   "character", NA, NA, NA,
                    desc = "Path to SCANFI 5x5 dir or base folder (optional; downloader can be used inside prepareCanopyModelData)"),
    defineParameter("pspDataPath", "character", NA, NA, NA,
                    desc = "Optional path to preprocessed PSP (RDS/CSV). If not provided, PSP will be downloaded & processed.")
  ),
  inputObjects = bindrows(
    expectsInput("studyAreaRas", "SpatRaster", 
                 desc = "Study area raster to match output to"),
    expectsInput("pspData", "data.table", 
                 desc = "Preprocessed PSP data with stand_age, biomass, vegetation height, canopy closure etc."),
    expectsInput("cohortData", "data.table", 
                 desc = "Cohort table from LandR"),
    expectsInput("pixelGroupMap", "SpatRaster", 
                 desc = "Raster of pixel groups from LandR")
    
  ),
  outputObjects = bindrows(
    createsOutput("outRasters", "list", 
                  desc = "List of predicted vegetation height and canopy closure rasters by year"),
    createsOutput("height_model", "xgb.Booster", 
                  desc = "Trained model for height prediction"),
    createsOutput("closure_model", "xgb.Booster", 
                  desc = "Trained model for closure prediction")
  )
))

doEvent.bird_covariateTranslator <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    
    "init" = {
      # Run immediately
      sim <- scheduleEvent(sim, time(sim), "bird_covariateTranslator", "run")
    },
    
    "run" = {
      # Extract time range
      predictStartYear <- P(sim)$predictStartYear
      predictEndYear   <- P(sim)$predictEndYear
      predictInterval  <- P(sim)$predictInterval
      #browser()
      if (is.na(predictStartYear) || is.na(predictEndYear)) {
        stop("Please set predictStartYear and predictEndYear in setupProject() call")
      }
      
      # Prepare PSP and SCANFI data if not supplied
      if (!suppliedElsewhere("pspData", sim)) {
        sim$pspData <- Cache(prepareCanopyModelData, 
                             scanfi_dir = P(sim)$scanfi_dir,
                             psp_path = P(sim)$psp_path)
      }
      
      # Train the model
      # sim$model <- Cache(trainCanopyHeightModels, pspData = sim$pspData)
      sim$model <- list(
        height = Cache(trainCanopyheightModels, pspData = sim$pspData,useCV = FALSE)$height_model, #, nFolds = 2
        closure = Cache(trainCanopyheightModels, pspData = sim$pspData,useCV =FALSE)$closure_model # , nFolds = 2
      )
      # Schedule prediction
      yearsToPredict <- seq(predictStartYear, predictEndYear, by = predictInterval)
      for (yr in yearsToPredict) {
        sim <- scheduleEvent(sim, yr, "bird_covariateTranslator", "predict", eventPriority = 5)
      }
    },
    
    # "predict" = {
    #   # Predict using cohortData (LandR) if available
    #   if (!is.null(sim$cohortData) && !is.null(sim$pixelGroupMap)) {
    #     sim$canopyPredictions[[as.character(time(sim))]] <- 
    #       predictCanopyStructure_OneYear(cohortData = sim$cohortData,
    #                                            pixelGroupMap = sim$pixelGroupMap,
    #                                      height_model = sim$model$height, 
    #                                      closure_model = sim$model$closure,
    #                                      studyAreaRas = sim$studyAreaRas,
    #                                            year = time(sim))
    #   } else {
    #     stop("cohortData and pixelGroupMap are required to predict canopy structure in LandR-integrated mode.")
    #   }
    # }
    "predict" = {
      # Predict using cohortData (LandR) if available
      if (!is.null(sim$cohortData) && !is.null(sim$pixelGroupMap)) {
        yearStr <- as.character(time(sim))
        
        # Predict SCANFI height & closure
        predictions <- predictCanopyStructure_OneYear(
          cohortData     = sim$cohortData,
          pixelGroupMap  = sim$pixelGroupMap,
          height_model   = sim$model$height, 
          closure_model  = sim$model$closure,
          studyAreaRas   = sim$studyAreaRas,
          year           = time(sim)
        )
        
        # convert cohort data to biomass rasters
        biomass_1km <- makeBiomassRaster(sim$cohortData, sim$pixelGroupMap)
        biomass_5x5 <- terra::focal(biomass_1km, w = matrix(1, 5, 5), fun = mean, na.rm = TRUE)
        
        # Project biomass to 1km resolution
        biomass_1km  <- terra::project(biomass_1km, sim$studyAreaRas)
        biomass_5x5 <- terra::project(biomass_5x5, sim$studyAreaRas)
        browser()
        predictions$SCANFIbiomass_1km <- biomass_1km
        predictions$SCANFIbiomass_5x5 <- biomass_5x5
        
        # Save prediction to sim object
        sim$canopyPredictions[[yearStr]] <- predictions
      } else {
        stop("cohortData and pixelGroupMap are required to predict canopy structure in LandR-integrated mode.")
      }
    }
    
    
  )
  return(invisible(sim))
}



### template initialization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}
### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

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

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

ggplotFn <- function(data, ...) {
  ggplot2::ggplot(data, ggplot2::aes(TheSample)) +
    ggplot2::geom_histogram(...)
}

