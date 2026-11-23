## Run from the module directory: Rscript --vanilla tests/test-canopy-cache.R
## Exercises the actual model-cache calls with a counting fake, never XGBoost.
main <- function() {
  library(reproducible)
  root <- tempfile("bird-canopy-cache-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  oldOptions <- options(
    reproducible.useCache = TRUE, reproducible.cacheSaveFormat = "rds",
    reproducible.verbose = 0, reproducible.stopOnCacheMiss = FALSE
  )
  on.exit(options(oldOptions), add = TRUE)
  expressions <- parse("bird_covariateTranslator.R")
  init <- NULL
  for (expr in expressions) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        identical(expr[[2]], as.name("InitTranslator"))) init <- expr[[3]]
  }
  env <- new.env(parent = environment())
  init <- eval(init, env)
  calls <- list()
  for (expr in as.list(body(init))[-1L]) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        is.call(expr[[2]]) && identical(expr[[2]][[1]], as.name("$")) &&
        as.character(expr[[2]][[3]]) %in% c("height_xgb", "closure_xgb")) {
      calls[[length(calls) + 1L]] <- expr
    }
  }
  stopifnot(length(calls) == 2L)
  fits <- 0L
  env$trainCanopyXGB <- function(model_data, response, predictors, nFolds = 5,
                                figDir = NULL, seed = 123) {
    fits <<- fits + 1L
    list(response = response, figDir = figDir, fitNumber = fits)
  }
  env$cachePath <- function(sim) file.path(root, "cache")
  env$outputPath <- function(sim) sim$outputPath
  env$canopyXFolds <- 5
  env$height_feat <- env$closure_feat <- list(predictors = "biomass")
  env$sim <- list(
    canopyModelData = data.frame(biomass = c(1, 2)),
    outputPath = file.path(root, "outputs")
  )
  ## Seed the old, direct calls at their original project-wide figure paths.
  for (response in c("height_p90", "closure")) {
    figure <- if (response == "height_p90") "xgb_height" else "xgb_closure"
    Cache(
      env$trainCanopyXGB, model_data = env$sim$canopyModelData,
      response = response, predictors = "biomass", nFolds = 5,
      figDir = file.path(root, "outputs", "figs", figure),
      cachePath = env$cachePath(env$sim), .functionName = "trainCanopyXGB"
    )
  }
  stopifnot(fits == 2L)
  ## No fit may execute when evaluating production calls for different runs.
  options(reproducible.stopOnCacheMiss = TRUE)
  for (run in c("ELF_6.2.2/rep1", "ELF_3.1.1_2/rep5")) {
    env$sim$outputPath <- file.path(root, "outputs", "bird_runs", run)
    for (call in calls) eval(call, env)
    stopifnot(
      fits == 2L,
      identical(env$sim$height_xgb$figDir,
                file.path(root, "outputs", "figs", "xgb_height")),
      identical(env$sim$closure_xgb$figDir,
                file.path(root, "outputs", "figs", "xgb_closure"))
    )
  }
  options(reproducible.stopOnCacheMiss = FALSE)
  env$sim$canopyModelData$biomass[1] <- 99
  eval(calls[[1]], env)
  stopifnot(fits == 3L)
  env$canopyXFolds <- 6
  eval(calls[[1]], env)
  stopifnot(fits == 4L)
  env$height_feat$predictors <- "stand_age"
  eval(calls[[1]], env)
  stopifnot(fits == 5L)
  message("Original-key reuse across ELF/replicate outputs and changed-input checks passed; no XGBoost training.")
}
main()
