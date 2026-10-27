# 
# ## cross validation with k fold,
# trainCanopyheightModels <- function(pspData, trainPlots = NULL, testPlots = NULL, useCV = TRUE, nFolds = 5) {
#   library(caret)
#   library(xgboost)
#   message("Preparing training data to train model")
#   psp <- copy(pspData)
#   psp[, B := biomass]
#   psp[, age := stand_age]
#   
#   # If train/test plots not provided, generate randomly while maintaining the logic of not using same plot for train and test data
#   if (is.null(trainPlots) || is.null(testPlots)) {
#     set.seed(123)
#     uniquePlots <- unique(psp$OrigPlotID1)
#     trainPlots <- sample(uniquePlots, size = floor(0.8 * length(uniquePlots)))
#     testPlots  <- setdiff(uniquePlots, trainPlots)
#     
#   }
#   browser()
#   # Filter training data
#   pspTrain <- psp[OrigPlotID1 %in% trainPlots]
#   pspTrain <- pspTrain[complete.cases(age, B ,height, closure)] #,broadleaf_prop 
#   pspTrain[, c("age", "B") := lapply(.SD, scale), .SDcols = c("age", "B")]
#   
#   if (useCV) {
#     message("Training with cross-validation using grouped folds")
#     # Create grouped folds by plot ID
#     folds <- groupKFold(pspTrain$OrigPlotID1, k = 2)
#     ctrl <- trainControl(method = "cv", index = folds, verboseIter = FALSE)
#     
#     # Train height model with caret
#     height_model <- train(
#       x = as.matrix(pspTrain[, .(age, B)]), #,broadleaf_prop
#       y = pspTrain$height,
#       method = "xgbTree",
#       trControl = ctrl,
#       tuneLength = 3  # can increase for more tuning
#     )
#     
#     # Train closure model similarly if needed
#     closure_model <- train(
#       x = as.matrix(pspTrain[, .(age, B)]), #,broadleaf_prop
#       y = pspTrain$closure,
#       method = "xgbTree",
#       trControl = ctrl,
#       tuneLength = 3
#     )
#   } else {
#     message("Training without cross-validation")
#     X_train <- as.matrix(pspTrain[, .(age, B)]) #,broadleaf_prop
#     closure_model <- xgboost(
#       data = xgb.DMatrix(X_train, label = pspTrain$closure),
#       nrounds = 100,
#       learning_rate = 0.05,
#       max_depth = 6,
#       objective = "reg:squarederror",
#       eval_metric = "rmse",
#       verbose = 1
#     )
#     height_model <- xgboost(
#       data = xgb.DMatrix(X_train, label = pspTrain$height),
#       nrounds = 100,
#       learning_rate = 0.05,
#       max_depth = 6,
#       objective = "reg:squarederror",
#       eval_metric = "rmse",
#       verbose = 1
#     )
#   }
#   
#   # Optional testing
#   if (!is.null(testPlots)) {
#     pspTest <- pspData[OrigPlotID1 %in% testPlots]
#     pspTest[, B := biomass]
#     pspTest[, age := stand_age]
#     pspTest <- pspTest[complete.cases(age, B,height, closure)] #, broadleaf_prop
#     pspTest[, c("age", "B") := lapply(.SD, scale), .SDcols = c("age", "B")]
#     X_test <- as.matrix(pspTest[, .(age, B)]) #,broadleaf_prop
#     
#     if (useCV) {
#       closure_pred_test <- predict(closure_model, X_test)
#       height_pred_test  <- predict(height_model, X_test)
#     } else {
#       closure_pred_test <- predict(closure_model, X_test)
#       height_pred_test  <- predict(height_model, X_test)
#     }
#     
#     closure_rmse <- sqrt(mean((closure_pred_test - pspTest$closure)^2))
#     height_rmse  <- sqrt(mean((height_pred_test - pspTest$height)^2))
#     
#     # Observed vs Predicted for Height
#     plot_height <- ggplot(data.frame(obs = pspTest$height, pred = height_pred_test),
#                           aes(x = obs, y = pred)) +
#       geom_point(alpha = 0.6) +
#       geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
#       theme_minimal(base_size = 13) +
#       labs(title = sprintf("Height: Observed vs Predicted (RMSE = %.2f)", height_rmse),
#            x = "Observed Height (m)", y = "Predicted Height (m)")
#     
#     # Observed vs Predicted for Closure
#     plot_closure <- ggplot(data.frame(obs = pspTest$closure, pred = closure_pred_test),
#                            aes(x = obs, y = pred)) +
#       geom_point(alpha = 0.6) +
#       geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
#       theme_minimal(base_size = 13) +
#       labs(title = sprintf("Closure: Observed vs Predicted (RMSE = %.2f)", closure_rmse),
#            x = "Observed Closure (%)", y = "Predicted Closure (%)")
#     
#     print(plot_height)
#     print(plot_closure)
#     
#     message(sprintf("Test RMSE – Closure: %.2f | Height: %.2f", closure_rmse, height_rmse))
#   }
#   
#   return(list(height_model = height_model, closure_model = closure_model))
# }

trainCanopyheightModels <- function(pspData, trainPlots = NULL, testPlots = NULL, useCV = TRUE, nFolds = 5) {
  library(data.table)
  library(xgboost)
  library(ggplot2)
  
  message("Preparing training data")
  psp <- copy(pspData)
  psp[, B := biomass]
  psp[, age := stand_age]
  
  # Generate train/test plots if not provided
  if (is.null(trainPlots) || is.null(testPlots)) {
    set.seed(123)
    uniquePlots <- unique(psp$OrigPlotID1)
    trainPlots <- sample(uniquePlots, size = floor(0.8 * length(uniquePlots)))
    testPlots  <- setdiff(uniquePlots, trainPlots)
  }
  
  # Filter training data
  pspTrain <- psp[OrigPlotID1 %in% trainPlots]
  pspTrain <- pspTrain[complete.cases(age, B, height, closure)]
  pspTrain[, c("age", "B") := lapply(.SD, scale), .SDcols = c("age", "B")]
  
  X_train <- as.matrix(pspTrain[, .(age, B)])
  y_height  <- pspTrain$height
  y_closure <- pspTrain$closure
  
  # Parameters
  params <- list(objective = "reg:squarederror", eval_metric = "rmse", eta = 0.05, max_depth = 6)
  nrounds <- 200
  
  # --------------------- CROSS-VALIDATION ---------------------
  if (useCV) {
    message("Training with manual grouped cross-validation")
    uniquePlotsTrain <- unique(pspTrain$OrigPlotID1)
    folds <- split(uniquePlotsTrain, cut(seq_along(uniquePlotsTrain), nFolds, labels = FALSE))
    
    cv_height_rmse  <- numeric(nFolds)
    cv_closure_rmse <- numeric(nFolds)
    
    for (i in seq_len(nFolds)) {
      message(sprintf("Fold %d / %d", i, nFolds))
      
      valPlots <- folds[[i]]
      trainMask <- !pspTrain$OrigPlotID1 %in% valPlots
      valMask   <- pspTrain$OrigPlotID1 %in% valPlots
      
      Xtr <- X_train[trainMask, ]
      Xva <- X_train[valMask, ]
      
      ytr_h <- y_height[trainMask]
      yva_h <- y_height[valMask]
      
      ytr_c <- y_closure[trainMask]
      yva_c <- y_closure[valMask]
      
      # Create DMatrix
      dtrain_h <- xgb.DMatrix(Xtr, label = ytr_h)
      dval_h   <- xgb.DMatrix(Xva, label = yva_h)
      
      dtrain_c <- xgb.DMatrix(Xtr, label = ytr_c)
      dval_c   <- xgb.DMatrix(Xva, label = yva_c)
      
      # Train height
      model_h <- xgb.train(params, dtrain_h, nrounds = nrounds, watchlist = list(val = dval_h), verbose = 0)
      pred_h <- predict(model_h, Xva)
      cv_height_rmse[i] <- sqrt(mean((pred_h - yva_h)^2))
      
      # Train closure
      model_c <- xgb.train(params, dtrain_c, nrounds = nrounds, watchlist = list(val = dval_c), verbose = 0)
      pred_c <- predict(model_c, Xva)
      cv_closure_rmse[i] <- sqrt(mean((pred_c - yva_c)^2))
    }
    
    message("CV RMSE (Height): ", paste(round(cv_height_rmse, 3), collapse = ", "))
    message("CV RMSE (Closure): ", paste(round(cv_closure_rmse, 3), collapse = ", "))
    
    # Train final models on full training data
    dtrain_full_h <- xgb.DMatrix(X_train, label = y_height)
    dtrain_full_c <- xgb.DMatrix(X_train, label = y_closure)
    
    height_model <- xgb.train(params, dtrain_full_h, nrounds = nrounds, verbose = 0)
    closure_model <- xgb.train(params, dtrain_full_c, nrounds = nrounds, verbose = 0)
    
  } else {
    message("Training without cross-validation")
    dtrain_h <- xgb.DMatrix(X_train, label = y_height)
    dtrain_c <- xgb.DMatrix(X_train, label = y_closure)
    
    height_model <- xgb.train(params, dtrain_h, nrounds = nrounds, verbose = 0)
    closure_model <- xgb.train(params, dtrain_c, nrounds = nrounds, verbose = 0)
  }
  
  # --------------------- TESTING ---------------------
  if (!is.null(testPlots)) {
    pspTest <- psp[OrigPlotID1 %in% testPlots]
    pspTest <- pspTest[complete.cases(age, B, height, closure)]
    pspTest[, c("age", "B") := lapply(.SD, scale), .SDcols = c("age", "B")]
    
    X_test <- as.matrix(pspTest[, .(age, B)])
    
    height_pred_test  <- predict(height_model, X_test)
    closure_pred_test <- predict(closure_model, X_test)
    
    height_rmse  <- sqrt(mean((height_pred_test - pspTest$height)^2))
    closure_rmse <- sqrt(mean((closure_pred_test - pspTest$closure)^2))
    
    message(sprintf("Test RMSE – Closure: %.2f | Height: %.2f", closure_rmse, height_rmse))
    
    # Plots
    plot_height <- ggplot(data.frame(obs = pspTest$height, pred = height_pred_test),
                          aes(x = obs, y = pred)) +
      geom_point(alpha = 0.6) +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
      theme_minimal(base_size = 13) +
      labs(title = sprintf("Height: Observed vs Predicted (RMSE = %.2f)", height_rmse),
           x = "Observed Height (m)", y = "Predicted Height (m)")
    
    plot_closure <- ggplot(data.frame(obs = pspTest$closure, pred = closure_pred_test),
                           aes(x = obs, y = pred)) +
      geom_point(alpha = 0.6) +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
      theme_minimal(base_size = 13) +
      labs(title = sprintf("Closure: Observed vs Predicted (RMSE = %.2f)", closure_rmse),
           x = "Observed Closure (%)", y = "Predicted Closure (%)")
    
    print(plot_height)
    print(plot_closure)
  }
  
  return(list(height_model = height_model, closure_model = closure_model))
}


