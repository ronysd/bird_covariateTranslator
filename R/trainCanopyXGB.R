trainCanopyXGB <- function(
    model_data,
    response,
    predictors,
    nFolds = 5,
    figDir = NULL,
    seed = 123
) {
  
  stopifnot(response %in% names(model_data))
  stopifnot(all(predictors %in% names(model_data)))
  
  library(data.table)
  library(reproducible)
  
  set.seed(seed)
  
  
  ## build XGB training table
  ## ------------------------------------------------------------
  
  dat_xgb <- as.data.table(model_data)[,
                                       c(response, "OrigPlotID1", "X","Y", predictors),  ## adding X and Y for spatial K fold
                                       with = FALSE
  ]
  
  dat_xgb <- na.omit(dat_xgb)
  
  if (nrow(dat_xgb) < 100) {
    stop("Too few observations for XGBoost training.")
  }
  
  ## stable digest for caching
  dig <- reproducible::.robustDigest(
    list(response, predictors, dat_xgb)
  )
  
  ## train model using xgboost wrapper
  ## ------------------------------------------------------------
  #browser()
  res <- runXGBOOST(
    dat          = dat_xgb,
    dig          = dig,
    nFolds       = nFolds,
    colnamesResp = response,
    SHAPthresh   = 0,
    figDir       = figDir,
    objective    = NULL,
    eval_metric  = NULL
  )
  
  message(sprintf(
    "Finished XGBoost training for '%s'", response
  ))
  
  browser()
  return(list(
    response   = response,
    predictors = res$final_model$predictors,
    model      = res$final_model$mod, 
    final_R2 = res$final_model$'R2', 
    final_RMSE = res$final_model$RMSE, 
    digest     = dig
  ))
}
