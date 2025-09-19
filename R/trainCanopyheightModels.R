
## cross validation with k fold,
trainCanopyheightModels <- function(pspData, trainPlots = NULL, testPlots = NULL, useCV = TRUE, nFolds = 5) {
  library(caret)
  library(xgboost)
  message("Preparing training data to train model")
  psp <- copy(pspData)
  psp[, B := biomass]
  psp[, age := stand_age]
  
  # If train/test plots not provided, generate randomly while maintaining the logic of not using same plot for train and test data
  if (is.null(trainPlots) || is.null(testPlots)) {
    set.seed(123)
    uniquePlots <- unique(psp$OrigPlotID1)
    trainPlots <- sample(uniquePlots, size = floor(0.8 * length(uniquePlots)))
    testPlots  <- setdiff(uniquePlots, trainPlots)
    
  }
  #browser()
  # Filter training data
  pspTrain <- psp[OrigPlotID1 %in% trainPlots]
  pspTrain <- pspTrain[complete.cases(age, B ,height, closure_SCANFI)] #,broadleaf_prop 
  pspTrain[, c("age", "B") := lapply(.SD, scale), .SDcols = c("age", "B")]
  
  if (useCV) {
    message("Training with cross-validation using grouped folds")
    # Create grouped folds by plot ID
    folds <- groupKFold(pspTrain$OrigPlotID1, k = nFolds)
    ctrl <- trainControl(method = "cv", index = folds, verboseIter = FALSE)
    
    # Train height model with caret
    height_model <- train(
      x = as.matrix(pspTrain[, .(age, B)]), #,broadleaf_prop
      y = pspTrain$height,
      method = "xgbTree",
      trControl = ctrl,
      tuneLength = 3  # can increase for more tuning
    )
    
    # Train closure model similarly if needed
    closure_model <- train(
      x = as.matrix(pspTrain[, .(age, B)]), #,broadleaf_prop
      y = pspTrain$closure_SCANFI,
      method = "xgbTree",
      trControl = ctrl,
      tuneLength = 3
    )
  } else {
    message("Training without cross-validation")
    X_train <- as.matrix(pspTrain[, .(age, B)]) #,broadleaf_prop
    closure_model <- xgboost(
      data = xgb.DMatrix(X_train, label = pspTrain$closure_SCANFI),
      nrounds = 100,
      learning_rate = 0.05,
      max_depth = 6,
      objective = "reg:squarederror",
      eval_metric = "rmse",
      verbose = 1
    )
    height_model <- xgboost(
      data = xgb.DMatrix(X_train, label = pspTrain$height),
      nrounds = 100,
      learning_rate = 0.05,
      max_depth = 6,
      objective = "reg:squarederror",
      eval_metric = "rmse",
      verbose = 1
    )
  }
  
  # Optional testing
  if (!is.null(testPlots)) {
    pspTest <- pspData[OrigPlotID1 %in% testPlots]
    pspTest[, B := biomass]
    pspTest[, age := stand_age]
    pspTest <- pspTest[complete.cases(age, B,height, closure_SCANFI)] #, broadleaf_prop
    pspTest[, c("age", "B") := lapply(.SD, scale), .SDcols = c("age", "B")]
    X_test <- as.matrix(pspTest[, .(age, B)]) #,broadleaf_prop
    
    if (useCV) {
      closure_pred_test <- predict(closure_model, X_test)
      height_pred_test  <- predict(height_model, X_test)
    } else {
      closure_pred_test <- predict(closure_model, X_test)
      height_pred_test  <- predict(height_model, X_test)
    }
    
    closure_rmse <- sqrt(mean((closure_pred_test - pspTest$closure_SCANFI)^2))
    height_rmse  <- sqrt(mean((height_pred_test - pspTest$height)^2))
    
    message(sprintf("Test RMSE – Closure: %.2f | Height: %.2f", closure_rmse, height_rmse))
  }
  
  return(list(height_model = height_model, closure_model = closure_model))
}
