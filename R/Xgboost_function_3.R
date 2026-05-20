
#' Wrapper for XGBoost
#'
#' Tune, fits and tests XGBoost models with
#' k-fold cross-validation., LATER modified to do tuning and cross validation with K-mean cluster on LAT and Long, mimicing the spatial clustering
#'
#' @param dat a `data.table` containing predictors and response variable.
#' @param dig a digest passed to `Cache(..., .cacheExtra)` to bypass digesting
#'    `dat`. Often `dig` is a digest of `dat`.
#' @param nFolds number of folds for cross-validating the final model
#'    (i.e. the model using tuned parameters).
#' @param colnamesResp Name of column in `dat` to use as response variable.
#'    All other columns will be used as predictors
#' @param interaction_constraints passed to `xgboost::xgboost`.
#'    By default no interaction constraints.
#' @param SHAPthresh. Quantile threshold used for feature (i.e. variable) selection
#'    based on SHAP values. Features with SHAP values below the quantile threshold
#'    are excluded and the model re-run. A warning is issued if this resulted in poorer
#'    performace (based on AUC score), in which case one may consider relaxing (i.e. lowering)
#'    the threshold.
#' @param figDir if not `NULL`, diagnostic tuning plots will be saved to this directory.
#'
#' @return a list (one entry per fold) of lists with:
#'   * `$mod`: fitted model
#'   * `shap_values`: SHAP values for the fitted model
#'   * `shap_long`: long version of the SHAP values for the fitted model (used for plotting)
#' @importFrom caret createFolds
#' @importFrom purrr pmap
#' @importFrom pROC roc
#' @importFrom crayon cyan
#' @importFrom SHAPforxgboost shap.values
#' @importFrom reproducible Cache
runXGBOOST <- function(dat, dig, nFolds = 5, colnamesResp = "height_p90", eval_metric = NULL, #"SEV_PROP", ## ## NEW ADDITION ADDED eval Mat as argument for regresson 
                       interaction_constraints = NULL, SHAPthresh = 0, objective = NULL,
                       figDir = NULL) {
  # dat <- dat[sample(NROW(dat), size = 1e4)]
  # tt <- table(dat$SEV_PROP)
  
  ## NEW ADDITION  START
  ##classification or regression
  is_classification <- is.factor(dat[[colnamesResp]]) 
  if (is.null(objective)) {
    objective <- if (is_classification) "binary:logistic" else "reg:squarederror"
  }
  
  if (is.null(eval_metric)) {
    eval_metric <- if (is_classification)
      c("auc", "logloss")
    else
      c("rmse", "mae", "rmsle")
  }
  ## NEW ADDITION END
  
  # # Add dummy variables for factor columns -- i.e., the random effects
  # if (all(sapply(dat, is.numeric)) %in% FALSE)
  #   dat <- model.matrix(~ . + 0, data = dat) |>
  #     Cache(omitArgs = c("object", "data", "x"),
  #           .cacheExtra = dig) # Creates dummy variables
  # 
  # dat <- as.data.table(dat)
  
  
  ## NEW ADDITION START
  
  if ("OrigPlotID1" %in% names(dat)) {
    plotID <- dat$OrigPlotID1       # save for grouped CV
    dat_noID <- dat[, !c("OrigPlotID1"), with = FALSE]
  } else {
    plotID <- NULL
    dat_noID <- dat
  }
  
  ## THEN apply model.matrix ONLY to dat_noID
  if (all(sapply(dat_noID, is.numeric)) %in% FALSE)
    dat_noID <- model.matrix(~ . + 0, data = dat_noID) |>
      Cache(omitArgs = c("object", "data", "x"),
            .cacheExtra = dig)
  
  dat_noID <- as.data.table(dat_noID)
  
  ## NEW ADDITION END  
  
  ## NEW ADDITION START
  
  # Remove PlotID from predictors (but keep separately for grouped CV) ## replace OrigPlotID1 with the respective Plot ID column name
  # if ("OrigPlotID1" %in% names(dat)) {
  #   plotID <- dat$OrigPlotID1  # save for groupKFold
  #   dat_noID <- dat[, !c("OrigPlotID1"), with = FALSE]
  # } else {
  #   plotID <- NULL
  #   dat_noID <- dat
  # }
  ## NEW ADDITION END
  
  colnamesPred <- setdiff(colnames(dat_noID), colnamesResp) ## after model.matrix bcs colnames change #dat_noID
  browser()
  ## excluding X and Y
  colnamesPred <- setdiff(colnamesPred, c("X", "Y"))
  
  ## Setup k-folds -----
  savedSeed <- .Random.seed
  on.exit(assign(".Random.seed", savedSeed, envir = .GlobalEnv), add = TRUE)
  set.seed(12345) # so kfolds are same, so Caching works correctly below; if dat changes number of rows,
  # it will be a totally different sequence; but it will be the same sequence
  # if number of rows doesn't change
  
  yearColname <- grep("year", tolower(colnames(dat_noID)), value = TRUE) #dat_noID
  indexNames <- c("allData", "evalData")
  #browser()
  if (length(yearColname)) {
    crossValType <- "time-ordered"
    times <- unique(dat_noID[[yearColname]])
    testLength <- 3
    initialWindow <- length(times) - testLength - nFolds + 1
    trainIndexK <- createTimeSlices(times, initialWindow = initialWindow, testLength, fixedWindow = FALSE)
    trainIndexK <- Map(tr = trainIndexK$train, te = trainIndexK$test, function(tr, te) {
      allData <- which(dat_noID[[yearColname]] %in% times[c(tr, te)])
      evalData <- which(dat_noID[[yearColname]] %in% times[te])
      list(allData, evalData) |> setNames(indexNames)
    })
  } else {
    crossValType <- "crossValidation"
    ## create folds and make a list with indices of full dataset and eahc fold
    # trainIndexK <- createFolds(dat[[colnamesResp]], k = nFolds, list = TRUE, returnTrain = FALSE)
    # trainIndexK <- Map(tr = trainIndexK, function(tr) {
    #   list(seq(NROW(dat)), tr) |> setNames(indexNames)
    # })
    
    ### NEW ADDITION START
    # folds <- groupKFold(plotID, k = nFolds)
    # 
    # trainIndexK <- lapply(folds, function(testIDs) {
    #   list(
    #     allData = seq_len(nrow(dat_noID)),
    #     evalData = testIDs
    #   )
    # })
    
    ## nEW add for spatial cross validation
    
    library(stats)
    
    coords <- as.matrix(dat[, .(X, Y)])   # use original dat, not dat_noID
    
    set.seed(12345)
    km <- kmeans(coords, centers = nFolds, nstart = 25)
 ## 24th march start   
    # fold_ids <- split(seq_len(nrow(dat)), km$cluster)
    # 
    # trainIndexK <- lapply(fold_ids, function(testIDs) {
    #   list(
    #     allData = seq_len(nrow(dat_noID)),
    #     evalData = testIDs
    #   )
    # })
    fold_ids <- split(seq_len(nrow(dat)), km$cluster)
    ## change 10th APril 2026
    
    # trainIndexK <- lapply(fold_ids, function(testIDs) {
    #   # identify plots in test
    #   testPlots <- unique(plotID[testIDs])
    #   # remove ALL rows of those plots from training
    #   trainIDs <- which(!plotID %in% testPlots)
    #   list(
    #     allData = trainIDs,
    #     evalData = testIDs
    #   )
    # })
    trainIndexK <- lapply(fold_ids, function(testIDs) {
      testPlots <- unique(plotID[testIDs])
      trainIDs <- which(!plotID %in% testPlots)
      
      # xgboost eval_set must be indexed relative to x = datPreds[allDataIDs]
      allDataIDs <- c(trainIDs, testIDs)
      evalData <- match(testIDs, allDataIDs)
      
      list(
        allData = allDataIDs,
        evalData = evalData,
        testIDs_raw = testIDs
      )
    })
    
## 24rth march End    
    # trainIndexK <- lapply(folds, function(testIDs) {
    #   trainIDs <- setdiff(seq_len(nrow(dat_noID)), testIDs)
    #   list(
    #     allData = trainIDs,
    #     evalData = testIDs
    #   )
    # })
    ### NEW ADDITION END
  }
  
  ## sample columns after setting seed for caching (if different, then cache is triggered)
  colOrder <- setdiff(colnames(dat_noID), c(yearColname))
  colOrder <- sample(colOrder)
  dat_noID <- dat_noID[, ..colOrder]
  dig <- .robustDigest(dat_noID)
  
  browser()
  #### for spatial k mean clustering
  coords <- dat[, .(X, Y)]  # or X, Y projected coords

  set.seed(123)
  km <- kmeans(scale(coords), centers = nFolds)

  spatialFoldID <- km$cluster
  
  ## Trying spatial block CV
  
  # library(blockCV)
  # 
  # sf_dat <- st_as_sf(dat, coords = c("X","Y"), crs = 5072)
  # 
  # sb <- spatialBlock(
  #   speciesData = sf_dat,
  #   k = nFolds,
  #   selection = "random",
  #   iteration = 100
  # )
  # 
  # spatialFolds <- list(
  #   index = sb$folds,
  #   indexOut = lapply(sb$folds, function(x)
  #     setdiff(seq_len(nrow(dat)), x))
  # )
  # 
  
  ## ------------------------------------------------------------
  ## Create Spatial CV folds using k-means clustering on X/Y
  ## ------------------------------------------------------------
  
  if (all(c("X", "Y") %in% names(dat))) {
    
    coords <- as.matrix(dat[, .(X, Y)])
    
    set.seed(123)
    km <- kmeans(coords, centers = nFolds)
    
    test_folds <- lapply(1:nFolds, function(k) {
      which(km$cluster == k)
    })
## 24th March Start
    
    # spatialFolds <- list(
    #   indexOut = test_folds,
    #   index = lapply(test_folds, function(testIDs) {
    #     setdiff(seq_len(nrow(dat_noID)), testIDs)
    #   })
    # )
    
    spatialFolds <- list(
      indexOut = test_folds,
      index = lapply(test_folds, function(testIDs) {
        testPlots <- unique(plotID[testIDs])
        which(!plotID %in% testPlots)
      })
    )  
  } else {
    
    spatialFolds <- NULL
    
  }
  
  
  
  ## Tune parameters on full data with caret first ----
  params <- .tunexgboost(dig,
                         dat_noID[, .SD, .SDcols = c(colnamesPred, colnamesResp)],  #dat_noID
                         colnamesResp = colnamesResp, spatialFolds = spatialFolds,
                         figDir) |>
    Cache(omitArgs = c("dat", "figDir"),
          #cacheId = "e95a84d83bac39e9"
    )
  
  ## subset predictor data
  datPreds <- dat_noID[, ..colnamesPred]  #dat_noID
  
  ## save feature names, as ecoprovince is now dummy coded, we need the name for each dummy coded column
  feature_template <- colnames(datPreds)
  
  
  st <- system.time(
    mm <- pmap(
      list(dataFolds = trainIndexK, kFold = seq(nFolds)),
      function(dataFolds, kFold) {
        ## get row IDs for training data (allData) and testing data
        
        ## change april 10th 2026
        
        # allDataIDs <- dataFolds[[indexNames[[1]]]]
        # testIDs <- dataFolds[[indexNames[[2]]]]   ## eval data
        allDataIDs <- dataFolds[[indexNames[[1]]]]
        testIDs <- dataFolds[[indexNames[[2]]]]        # relative positions for eval_set
        raw_testIDs <- dataFolds[["testIDs_raw"]]      # original full-data row ids
        
        dig2 <- .robustDigest(dataFolds)
        
        # xgboost objects do not save with `qs` ... must be `rds`
        # opt <- options(reproducible.cacheSaveFormat = "rds")
        # on.exit(options(opt)) # redundant; but necessary if it fails during fit
        lowSHAPcols <- 1
        calcThresh <- TRUE
        # SHAPthresh <- 0.25  ## test
        cols2keep <- colnames(datPreds)
        
        modOut <- NULL
        
        while (length(lowSHAPcols)) {
          ## TODO: test: go back to previous model if AUC decreases after removing features
          #browser()
          modOut2 <- xgboost(x = datPreds[allDataIDs],
                             , y = dat_noID[[colnamesResp]][allDataIDs] #dat_noID
                             , interaction_constraints = interaction_constraints
                             # , objective = "reg:tweedie" ## no improvements
                             , nthread = 10
                             , eval_set = testIDs,
                             , monitor_training = TRUE
                             , eval_metric = eval_metric    #c("auc", "rmse", "logloss")   ### NEW ADDITION
                             , objective = objective, ### NEW ADDITION
                             , early_stopping_rounds = 100
                             , max_depth = params$max_depth   ## improved fit.
                             , nrounds = params$nrounds
                             , learning_rate = params$eta
                             , min_split_loss = params$gamma
                             , min_child_weight = params$min_child_weight
                             , colsample_bytree = params$colsample_bytree
                             
          ) |>
            Cache(omitArgs = c("x", "y", "eval_set"),
                  .functionName = .functionNameHelper("xgboost", kFold),
                  .cacheExtra = c(dig, dig2, cols2keep),
                  showSimilar = TRUE,
                  cacheSaveFormat = "rds")
          
          # if (is.null(modOut)) {
          #   modOut <- modOut2
          #   AUCout <- tail(attr(modOut, "evaluation_log"), 1)$train_auc
          # }
          # 
          # ## get last AUC
          # AUCout2 <- tail(attr(modOut2, "evaluation_log"), 1)$train_auc
          # if (AUCout2 < AUCout) {
          #   message("AUC decreased after removing features.\n",
          #           "  The previous model will be retained, instead")
          # } else {
          #   modOut <- modOut2
          # }
          # AUCout <- AUCout2
          
          ### NEW ADDITION START
          eval_log2 <- attr(modOut2, "evaluation_log")
          last_row <- eval_log2[nrow(eval_log2), ]
          
          if (is_classification) {
            auc_col <- grep("auc", names(last_row), value = TRUE)
            AUCout2 <- if (length(auc_col)) as.numeric(last_row[[auc_col]]) else NA
            
            if (is.null(modOut)) {
              modOut <- modOut2
              AUCout <- AUCout2
            } else if (!is.na(AUCout2) && AUCout2 >= AUCout) {
              modOut <- modOut2
              AUCout <- AUCout2
            }
            
          } else {
            # Regression: use RMSE for comparison instead of AUC
            rmse_col <- grep("^eval_rmse$", names(last_row), value = TRUE)
            
            RMSEout2 <- if (length(rmse_col) == 1) {
              as.numeric(last_row[[rmse_col]])
            } else {
              NA
            }
            
            if (is.null(modOut)) {
              modOut <- modOut2
              RMSEout <- RMSEout2
            } else if (!is.na(RMSEout2) && RMSEout2 <= RMSEout) {
              # smaller is better for RMSE
              modOut <- modOut2
              RMSEout <- RMSEout2
            }
          }
          
          ## calculate predictions and residuals
          ## change 10th april 2026
          
          # valData <- datPreds[testIDs,]
          # valData <- cbind(valData,
          #                  obs =  dat_noID[[colnamesResp]][testIDs], #dat_noID
          #                  pred = predict(modOut, datPreds[testIDs, ]))
          # valData[, resid := pred - obs]
          valData <- datPreds[raw_testIDs, ]
          valData <- cbind(
            valData,
            obs = dat_noID[[colnamesResp]][raw_testIDs],
            pred = predict(modOut, datPreds[raw_testIDs, ])
          )
          valData[, resid := pred - obs]
          
          #### NEW ADDITION START
          ### for TEST RMSE and R Square
          
          RMSE_fold <- sqrt(mean((valData$obs - valData$pred)^2))
          R2_fold <- 1 - sum((valData$obs - valData$pred)^2) /
            sum((valData$obs - mean(valData$obs))^2)
          
          
          ### NEW ADDITION END
          
          ## Feature selection -- remove features (variables) with low SHAP values
          ## based on a quantile threshold
          shap_values <- shap.values(modOut, datPreds) |>
            Cache(omitArgs = formalArgs(shap.values),
                  .functionName = .functionNameHelper("shap.values", "xgboost", kFold),
                  .cacheExtra = c(dig, dig2, cols2keep))
          meanSHAP <- shap_values$mean_shap_score
          
          if (calcThresh) {
            SHAPthresh <- quantile(meanSHAP, prob = SHAPthresh)
            calcThresh <- FALSE ## we only calculate the threshold once
          }
          
          lowSHAPcols <- names(which(meanSHAP < SHAPthresh))
          if (length(lowSHAPcols)) {
            cols2keep <- setdiff(colnames(datPreds), lowSHAPcols)
            datPreds <- datPreds[, ..cols2keep]
            
            message("Removing features with SHAP < ", SHAPthresh, ":\n",
                    paste(lowSHAPcols, collapse = ", "))
          }
          
        }
        
        ## more outputs
        shapContrib <- shap_values$shap_score
        shapContrib <- shapContrib[, -"(Intercept)"]
        shap_long <- shap.prep(shap_contrib = shapContrib, X_train = datPreds) |>
          Cache(omitArgs = formalArgs(shap.prep),
                .functionName = .functionNameHelper("shap.prep", kFold),
                .cacheExtra = c(dig, dig2, cols2keep))
        
        ## NEW ADDITION SHAP PLOT START
        
        # Common Plot directory
        plot_dir <- file.path(figDir, "fold_plots")
        
        p_shap <- shap.plot.summary(
          shap_long,
          scientific = FALSE
        )
        
        # save SHAP plot for this fold
        if (!is.null(figDir)) {
          dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
          #shap_dir <- file.path(figDir, paste0("fold_", kFold))
          #dir.create(shap_dir, recursive = TRUE, showWarnings = FALSE)
          # dir.create("figs/xgb_height/fold_plots", showWarnings = FALSE, recursive = TRUE)
          
          ggsave(
            filename = file.path(plot_dir, paste0("SHAPSummary_Fold", kFold, ".png")),
            plot = p_shap,
            height = 7,
            width = 6,
            dpi = 300
          )
        }
        
        ## NEW ADDITION SHAP PLOT END
        
        
        
        ## NEW ADDITION PLOT OBS vs PRED START
        
        
        ## ====== NEW: Observed vs Predicted plot per fold ======
        
        ## NEW PLOTS, START
        res_age_plot <- ggplot(valData, aes(x = stand_age, y = resid)) +
          geom_point(alpha = 0.5) +
          geom_smooth(method = "loess", colour = "blue", se = FALSE) +
          geom_hline(yintercept = 0, linetype = "dashed") +
          theme_bw(base_size = 14) +
          labs(
            title = paste0("Residual vs Stand Age (Fold ", kFold, ")"),
            x = "Stand age",
            y = "Residual (Pred - Obs)"
          )
        
        ggsave(
          filename = file.path(plot_dir, paste0("Residual_Age_Fold", kFold, ".png")),
          plot = res_age_plot,
          width = 6,
          height = 5,
          dpi = 300
        )
        
        res_biomass_plot <- ggplot(valData, aes(x = biomass, y = resid)) +
          geom_point(alpha = 0.5) +
          geom_smooth(method = "loess", colour = "blue", se = FALSE) +
          geom_hline(yintercept = 0, linetype = "dashed") +
          theme_bw(base_size = 14) +
          labs(
            title = paste0("Residual vs Biomass (Fold ", kFold, ")"),
            x = "Biomass",
            y = "Residual (Pred - Obs)"
          )
        
        ggsave(
          filename = file.path(plot_dir, paste0("Residual_Biomass_Fold", kFold, ".png")),
          plot = res_biomass_plot,
          width = 6,
          height = 5,
          dpi = 300
        )
        # change 10th april 2026
        #eco_vals <- dat$ECOPROVINC[testIDs]#dat$ECOPROVINC[testIDs]
        eco_vals <- dat$ECOPROVINC[raw_testIDs]
        eco_df <- data.frame(
          obs = valData$obs,
          pred = valData$pred,
          eco = factor(eco_vals)
        )
        
        eco_plot <- ggplot(eco_df, aes(obs, pred, colour = eco)) +
          geom_point(alpha = 0.6) +
          geom_abline(intercept = 0, slope = 1, colour = "black") +
          theme_bw(base_size = 14) +
          labs(
            title = paste0("Obs vs Pred by ECOPROVINC (Fold ", kFold, ")"), #Ecoprovince
            x = "Observed",
            y = "Predicted",
            colour = "ECOPROVINC" #Ecoprovince
          )
        
        ggsave(
          filename = file.path(plot_dir, paste0("ObsPred_Ecoprov_Fold", kFold, ".png")),
          plot = eco_plot,
          width = 7,
          height = 6,
          dpi = 300
        )
        
        ## NEW PLOTS, END
        
        plot_df <- data.frame(
          obs = valData$obs,
          pred = valData$pred
        )
        
        # Compute RMSE + R2 for printing
        rmse_val <- sprintf("RMSE = %.3f", RMSE_fold)
        r2_val   <- sprintf("R² = %.3f", R2_fold)
        
        p <- ggplot(plot_df, aes(x = obs, y = pred)) +
          geom_point(alpha = 0.5) +
          geom_abline(slope = 1, intercept = 0, color = "red", lwd = 1.2) +
          labs(
            title = paste0("Observed vs Predicted (Fold ", kFold, ")"),
            subtitle = paste(rmse_val, r2_val, sep = "   "),
            x = "Observed",
            y = "Predicted"
          ) +
          theme_bw(base_size = 14)
        
        # Create directory if needed
        # dir.create("figs/xgb_height/fold_plots", showWarnings = FALSE, recursive = TRUE)
        
        ggsave(
          # filename = paste0("figs/xgb_height/fold_plots/ObsPred_Fold", kFold, ".png"),
          filename = file.path(plot_dir, paste0("ObsPred_Fold", kFold, ".png")),
          plot = p,
          width = 6, height = 5, dpi = 300
        )
        
        
        
        ## NEW ADDITION END
        
        
        
        list(valData = valData,
             mod = modOut,
             shap_values = shap_values,
             shap_long = shap_long,
             RMSE = RMSE_fold, ## NEW ADDITION
             R2 = R2_fold,## NEW ADDITION
             OBSplot = p,## NEW ADDITION
             SHAP = p_shap,## NEW ADDITION
             residual_age = res_age_plot,
             residual_biomass = res_biomass_plot,
             eco_plot = eco_plot)
      })
  )
  #browser()

  ## FINAL MODEL (80–20 by PlotID)

  
  if (!is.null(plotID)) {
    
    set.seed(12345)
    
    # split by PlotID
    unique_plots <- unique(plotID)
    n_test_plots <- floor(0.2 * length(unique_plots))
    test_plots <- sample(unique_plots, n_test_plots)
    
    ## chnage 10th APril 2026
    
    # final_test_ids  <- which(plotID %in% test_plots)
    # final_train_ids <- setdiff(seq_len(nrow(dat_noID)), final_test_ids)
    final_test_ids  <- which(plotID %in% test_plots)
    final_train_ids <- which(!plotID %in% test_plots)
  } else {
    # fallback: row-based split
    set.seed(12345)
    n <- nrow(dat_noID)
    test_ids <- sample(seq_len(n), size = floor(0.2 * n))
    final_test_ids <- test_ids
    final_train_ids <- setdiff(seq_len(n), test_ids)
  }
  
  ## ---- Fit final model on TRAIN ----
  final_mod <- xgboost(
    x = datPreds[final_train_ids],
    y = dat_noID[[colnamesResp]][final_train_ids],
    nthread = 10,
    objective = objective,
    eval_metric = eval_metric,
    max_depth = params$max_depth,
    nrounds = params$nrounds,
    learning_rate = params$eta,
    min_split_loss = params$gamma,
    min_child_weight = params$min_child_weight,
    colsample_bytree = params$colsample_bytree,
    #early_stopping_rounds = 100,
    #eval_set = final_test_ids,
    monitor_training = TRUE
  )
  
  ## ---- Predictions on HELD-OUT TEST ----
  final_pred <- predict(final_mod, datPreds[final_test_ids])
  final_obs  <- dat_noID[[colnamesResp]][final_test_ids]
  
  final_RMSE <- sqrt(mean((final_obs - final_pred)^2))
  final_R2 <- 1 - sum((final_obs - final_pred)^2) /
    sum((final_obs - mean(final_obs))^2)
  
  final_valData <- data.table(
    obs = final_obs,
    pred = final_pred,
    resid = final_pred - final_obs
  )
  
  ## ---- SHAP for final model ----
  final_shap_values <- shap.values(final_mod, datPreds)
  final_shap_contrib <- final_shap_values$shap_score
  final_shap_contrib <- final_shap_contrib[, -"(Intercept)"]
  
  final_shap_long <- shap.prep(
    shap_contrib = final_shap_contrib,
    X_train = datPreds
  )
  
  final_shap_plot <- shap.plot.summary(final_shap_long, scientific = FALSE)
  
  ## ---- Obs vs Pred plot ----
  if (!is.null(figDir)) {
    final_dir <- file.path(figDir, "final_model")
    dir.create(final_dir, recursive = TRUE, showWarnings = FALSE)
  ## NEW PLOT START
  ## ---- Diagnostic plots for FINAL model ----
  
  # Extract predictors for final test set
  stand_age_test <- dat$stand_age[final_test_ids]
  biomass_test   <- dat$biomass[final_test_ids]
  eco_vals       <- dat$ECOPROVINC[final_test_ids] #ECOPROVINC
  
  diag_df <- data.frame(
    obs = final_valData$obs,
    pred = final_valData$pred,
    resid = final_valData$resid,
    stand_age = stand_age_test,
    biomass = biomass_test,
    eco = factor(eco_vals)
  )
  
  ## Residual vs Stand Age
  res_age_plot <- ggplot(diag_df, aes(x = stand_age, y = resid)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "loess", colour = "blue", se = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_bw(base_size = 14) +
    labs(
      title = "Final model: Residual vs Stand Age",
      x = "Stand age",
      y = "Residual (Pred - Obs)"
    )
  
  ggsave(
    filename = file.path(final_dir, "Residual_Age_Final.png"),
    plot = res_age_plot,
    width = 6,
    height = 5,
    dpi = 300
  )
  
  ## Residual vs Biomass
  res_biomass_plot <- ggplot(diag_df, aes(x = biomass, y = resid)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "loess", colour = "blue", se = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_bw(base_size = 14) +
    labs(
      title = "Final model: Residual vs Biomass",
      x = "Biomass",
      y = "Residual (Pred - Obs)"
    )
  
  ggsave(
    filename = file.path(final_dir, "Residual_Biomass_Final.png"),
    plot = res_biomass_plot,
    width = 6,
    height = 5,
    dpi = 300
  )
  
  ## Obs vs Pred coloured by Ecoprovince/Eoregion
  eco_plot <- ggplot(diag_df, aes(obs, pred, colour = eco)) +
    geom_point(alpha = 0.6) +
    geom_abline(intercept = 0, slope = 1, colour = "black") +
    theme_bw(base_size = 14) +
    labs(
      title = "Final model: Obs vs Pred by Ecoreigon", #Ecoprovince
      x = "Observed",
      y = "Predicted",
      colour = "ECOPROVINC" #Ecoprovince
    )
  
  ggsave(
    filename = file.path(final_dir, "ObsPred_Ecoprov_Final.png"),
    plot = eco_plot,
    width = 7,
    height = 6,
    dpi = 300
  )
  
  ##NEW PLOTS END
  
  final_plot <- ggplot(final_valData, aes(obs, pred)) +
    geom_point(alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0, colour = "red", linewidth = 1) +
    labs(
      title = "Final model: Observed vs Predicted (held-out 20%)",
      subtitle = paste0(
        "RMSE = ", round(final_RMSE, 3),
        "   R² = ", round(final_R2, 3)
      ),
      x = "Observed",
      y = "Predicted"
    ) +
    theme_bw(base_size = 14)
  
  ## ---- Save plots ----

    
    ggsave(
      file.path(final_dir, "ObsPred_Final.png"),
      final_plot, width = 6, height = 5, dpi = 300
    )
    
    ggsave(
      file.path(final_dir, "SHAPSummary_Final.png"),
      final_shap_plot, width = 6, height = 7, dpi = 300
    )
  }
  
  ## ---- Attach to output ----
  browser()
  mm$final_model <- list(
    mod = final_mod,
    valData = final_valData,
    predictors = feature_template,   # names for all the predictors
    RMSE = final_RMSE,
    R2 = final_R2,
    shap_values = final_shap_values,
    shap_long = final_shap_long,
    OBSplot = final_plot,
    SHAP = final_shap_plot,
    residual_age = res_age_plot,
    residual_biomass = res_biomass_plot,
    eco_plot = eco_plot
  )
  
  
  
  
  return(mm)
}

#' Tune XGBoost parameters with `caret`
#'
#' Tuning is done in 3 steps.
#' Step 1. Tune learning rate (called `learning_rate` in `xgboost`
#' and `eta` in `caret`)
#' Step 2. Take the best learning rate and tune all other parameters
#' (from those passed to `xgboost` by `[caret::train()]`) except
#' `rnounds` and `sample` (which is always kept as 1).
#' Step 3. Take the best parameters values from Steps 1 and 2 and tune
#' `nrounds`.
#'
#' @returns a data.frame of best parameter values.
#'
#' @inheritParams runXGBOOST
#' @importFrom caret trainControl train caretTheme
#' @importFrom reproducible Cache
#' @importFrom lattice trellis.par.set
.tunexgboost <- function(dig, dat, colnamesResp, spatialFolds = NULL, figDir) {
  ## use devtools::load_all("C:/Users/cbarros/GitHub/caret/pkg/caret/")
  ## bug reported at: https://github.com/topepo/caret/issues/1412
  savePlot <- FALSE
  if (!is.null(figDir)) {
    dir.create(figDir, showWarnings = FALSE, recursive = TRUE)
    savePlot <- TRUE
  }
  
  colnamesPred <- setdiff(colnames(dat), colnamesResp)
  
  ## Step 1. tune learning rate.
  ## eta = learning rate.
  param_grid1 <- data.frame(nrounds = 200,
                            eta = seq(0.01, 1, by =  0.01),
                            ## defaults in xgboost:
                            max_depth = 6,
                            gamma = 0,
                            colsample_bytree = 1,
                            min_child_weight = 1,
                            subsample = 1)
  
  # xgb_trcontrol <- trainControl(
  #   method = "cv",
  #   number = 5,
  #   verboseIter = TRUE,
  #   returnData = FALSE,
  #   returnResamp = "final",
  #   allowParallel = TRUE,
  #   savePredictions = "final"
  # )
  
  if (!is.null(spatialFolds)) {
    
    xgb_trcontrol <- trainControl(
      method = "cv",
      index = spatialFolds$index,
      indexOut = spatialFolds$indexOut,
      verboseIter = TRUE,
      returnData = FALSE,
      returnResamp = "final",
      allowParallel = TRUE,
      savePredictions = "final"
    )
    
  } else {
    
    xgb_trcontrol <- trainControl(
      method = "cv",
      number = 5,
      verboseIter = TRUE,
      returnData = FALSE,
      returnResamp = "final",
      allowParallel = TRUE,
      savePredictions = "final"
    )
  }
  
  
  message(cyan("Tuning learning rate..."))
  st <- system.time(
    {
      xgb_tuned <- train(x = as.data.frame(dat[, ..colnamesPred]),
                         y = dat[[colnamesResp]],
                         trControl = xgb_trcontrol,
                         tuneGrid = param_grid1,
                         method = "xgbTree"
      ) |>
        Cache(omitArgs = c("x", "y"),
              .functionName = .functionNameHelper("train", "tune_learningrate"),
              .cacheExtra = c(dig),
              showSimilar = TRUE,
              ## cacheId = "8a518e3d96830586",
              cacheSaveFormat = "rds")
    }
  )
  
  paramsF <- xgb_tuned$bestTune
  message(cyan("Finished in", st[["elapsed"]], "sec."))
  
  ## save tuning output
  if (savePlot) {
    png(file.path(figDir, "tuning_learningRate.png"), height = 4, width = 6,
        units = "in", res = 300)
    trellis.par.set(caretTheme())
    print(plot(xgb_tuned))
    dev.off()
  }
  
  ## Step 2. fix best learning rate and vary the rest
  param_grid2 <- expand.grid(nrounds = 200,
                             max_depth = c(1:10),
                             eta = paramsF$eta,
                             gamma = c(0, 0.1, 1, 2),#, 5, 10), ## tested with more initially, but not necessary
                             colsample_bytree = c(0.1, 0.5, 1),
                             min_child_weight = c(0, 1, 2, 5),
                             subsample = 1)
  
  ## tune other parameters
  for (i in 1:3) gc(reset = TRUE)
  message(cyan("Tuning remaining XGBoost parameters..."))
  st <- system.time(
    {
      xgb_tuned <- train(x = as.data.frame(dat[, ..colnamesPred]),
                         y = dat[[colnamesResp]],
                         trControl = xgb_trcontrol,
                         tuneGrid = param_grid2,
                         method = "xgbTree"
      ) |>
        Cache(omitArgs = c("x", "y"),
              .functionName = .functionNameHelper("train", "tune_all"),
              .cacheExtra = c(dig),
              showSimilar = TRUE,
              ## cacheId = "d76ffa84709d8db0",
              cacheSaveFormat = "rds")
    }
  )
  
  paramsF <- xgb_tuned$bestTune
  message(cyan("Finished in", st[["elapsed"]], "sec."))   ## about 4hrs
  
  ## save tuning output
  if (savePlot) {
    png(file.path(figDir, "tuning_all.png"), height = 12, width = 12,
        units = "in", res = 300)
    trellis.par.set(caretTheme())
    print(plot(xgb_tuned))
    dev.off()
  }
  
  ## Step 3. vary only no. rounds
  param_grid3 <- expand.grid(nrounds = c(100, 200, 500, 1000, 1500),
                             max_depth = paramsF$max_depth,
                             eta = paramsF$eta,
                             gamma = paramsF$gamma,
                             colsample_bytree = paramsF$colsample_bytree,
                             min_child_weight = paramsF$min_child_weight,
                             subsample = 1)
  
  ## tune other parameters
  for (i in 1:3) gc(reset = TRUE)
  message(cyan("Tuning no. rounds (trees)..."))
  st <- system.time(
    {
      xgb_tuned <- train(x = as.data.frame(dat[, ..colnamesPred]),
                         y = dat[[colnamesResp]],
                         trControl = xgb_trcontrol,
                         tuneGrid = param_grid3,
                         method = "xgbTree"
      ) |>
        Cache(omitArgs = c("x", "y"),
              .functionName = .functionNameHelper("train", "tune_nrounds"),
              .cacheExtra = c(dig),
              showSimilar = TRUE,
              ## cacheId = "42d9114a51b67432",
              cacheSaveFormat = "rds")
    }
  )
  
  paramsF <- xgb_tuned$bestTune
  message(cyan("Finished in", st[["elapsed"]], "sec."))
  message(cyan("Best parameters:"))
  message(cyan(paste0(capture.output(paramsF), collapse = "\n")))
  
  ## save tuning output
  if (savePlot) {
    png(file.path(figDir, "tuning_nrounds.png"), height = 4, width = 6,
        units = "in", res = 300)
    trellis.par.set(caretTheme())
    print(plot(xgb_tuned))
    dev.off()
  }
  
  for (i in 1:3) gc(reset = TRUE)
  return(paramsF)
}



.functionNameHelper <- function(..., sep = "_") {
  paste(..., sep = sep)
}

