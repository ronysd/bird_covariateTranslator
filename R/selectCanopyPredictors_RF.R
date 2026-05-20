selectCanopyPredictors_RF <- function(
    model_data,
    response,
    n_vars = NULL,
    corr_cutoff = 0.9,
    exclude_cols = c(
      "OrigPlotID1",
      "MeasureYear",
      "height_p90",
      "Elevation",
      "Elevation.x",
      "Elevation.y",
      "closure",
      "X",
      "Y",
      "scanfi_height"
    ),
    num_trees = 500,
    seed = 123
) {
  
  stopifnot(response %in% names(model_data))
  
  library(data.table)
  library(ranger)
  library(caret)
  
  set.seed(seed)
  
  dt <- as.data.table(model_data)
  
  
  ## define structural variables (those always retained, as opposed to climate variables)
  ## ------------------------------------------------------------
  
  structural_vars <- c(
    "biomass",
    "stand_age",
    "broadleaf_prop",
    "ECOPROVINC"#"ECOPROVINC"
    #
  )
  
  structural_vars <- intersect(structural_vars, names(dt))
  
  ## identify climate candidate predictors
  ## ------------------------------------------------------------
  
  predictor_candidates <- setdiff(
    names(dt),
    c(exclude_cols, response, structural_vars)
  )
  
  # Keep numeric only for RF + correlation
  predictor_candidates <- predictor_candidates[
    sapply(dt[, ..predictor_candidates], is.numeric)
  ]
  
  if (length(predictor_candidates) == 0) {
    stop("No valid numeric climate predictors found.")
  }
  
  
  ## bBuild RF dataset (response + climate only)
  ## ------------------------------------------------------------
  
  rf_dt <- dt[, c(response, predictor_candidates), with = FALSE]
  rf_dt <- rf_dt[!is.na(get(response))]
  rf_dt <- rf_dt[complete.cases(rf_dt)]
  
  if (nrow(rf_dt) < 50) {
    stop("Too few complete cases for RF modelling.")
  }
  
  ## remove zero-variance predictors
  ## ------------------------------------------------------------
  
  zero_var <- predictor_candidates[
    sapply(rf_dt[, ..predictor_candidates], function(x)
      sd(x, na.rm = TRUE) == 0)
  ]
  
  if (length(zero_var) > 0) {
    predictor_candidates <- setdiff(predictor_candidates, zero_var)
  }
  
  stopifnot(length(predictor_candidates) > 0)
  
  rf_dt <- rf_dt[, c(response, predictor_candidates), with = FALSE]
  
  
  ## correlation filtering (climate only)
  ## ------------------------------------------------------------
  
  if (!is.null(corr_cutoff)) {
    
    cor_mat <- cor(
      rf_dt[, ..predictor_candidates],
      use = "pairwise.complete.obs"
    )
    
    high_corr <- findCorrelation(
      cor_mat,
      cutoff = corr_cutoff,
      verbose = FALSE
    )
    
    if (length(high_corr) > 0) {
      predictor_candidates <- predictor_candidates[-high_corr]
    }
    
    stopifnot(length(predictor_candidates) > 0)
  }
  
  rf_dt <- rf_dt[, c(response, predictor_candidates), with = FALSE]
  
  
  ## random Forest on climate variables only
  ## ------------------------------------------------------------
  
  rf_formula <- as.formula(
    paste(response, "~", paste(predictor_candidates, collapse = "+"))
  )
  
  rf_model <- ranger(
    rf_formula,
    data = rf_dt,
    num.trees = num_trees,
    importance = "impurity",
    num.threads = max(1, parallel::detectCores() - 1)
  )
  
  ## rank importance
  ## ------------------------------------------------------------
  
  importance_dt <- data.table(
    variable = names(rf_model$variable.importance),
    importance = rf_model$variable.importance
  )[order(-importance)]
  
  if (is.null(n_vars)) {
    climate_selected <- importance_dt$variable
  } else {
    climate_selected <- importance_dt[
      1:min(n_vars, .N),
      variable
    ]
  }
  
  message(sprintf(
    "RF selected %d climate predictors for response '%s'",
    length(climate_selected), response
  ))
  
  ## final predictor set (add structural back)
  ## ------------------------------------------------------------
  
  final_predictors <- unique(c(structural_vars, climate_selected))
  
  stopifnot(all(final_predictors %in% names(model_data)))
  
  return(list(
    response = response,
    predictors = final_predictors,
    climate_selected = climate_selected,
    structural_vars = structural_vars,
    rf_importance = importance_dt
  ))
}
