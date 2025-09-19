predictClosureHeightFromCohort <- function(pspData, cohortData, pixelGroupMap, studyAreaRas,
                                           trainPlots = NULL, testPlots = NULL,
                                           modelHeight = NULL, modelClosure = NULL) {
  require(data.table)
  require(xgboost)
  require(terra)

  # Only train model if one is not supplied
  if (is.null(modelHeight) || is.null(modelClosure)) {
    message("Training models inside predictClosureHeightFromCohort() — not recommended for multi-year use")
    stop("must provide modelHeight and modelClosure for multi-year prediction")
  }

  # Step 1: Prepare cohort data
  cohort <- copy(cohortData)
  setnames(cohort, "pixelGroup", "pixelGroupID", skip_absent = TRUE)

  if (!all(c("age", "B") %in% names(cohort))) {
    stop("Cohort data must have 'age' and 'B' columns")
  }

  cohort <- cohort[!is.na(B) & B > 0]
  cohort[, B_raw := B]
  cohort[, c("age", "B") := lapply(.SD, scale), .SDcols = c("age", "B")]
  cohort_matrix <- as.matrix(cohort[, .(age, B)])

  #browser()
  # Predict using pre-trained models
  cohort[, predicted_closure := predict(modelClosure, newdata = cohort_matrix)]
  cohort[, predicted_height  := predict(modelHeight,  newdata = cohort_matrix)]

  # Aggregate to pixelGroup level
  pixel_stats <- cohort[, .(
    height = max(predicted_height, na.rm = TRUE),
    closure = sum(predicted_closure * B_raw, na.rm = TRUE) / sum(B_raw, na.rm = TRUE)
  ), by = pixelGroupID]

  # Rasterize
  pixelIndex <- as.data.table(terra::as.data.frame(pixelGroupMap, cells = TRUE))
  setnames(pixelIndex, old = names(pixelIndex)[2], new = "pixelGroupID")

  pixelIndex <- merge(pixelIndex, pixel_stats, by = "pixelGroupID", all.x = TRUE)

  r_height <- terra::rast(pixelGroupMap)
  r_closure <- terra::rast(pixelGroupMap)

  r_height[pixelIndex$cell] <- pixelIndex$height
  r_closure[pixelIndex$cell] <- pixelIndex$closure
  names(r_height) <- "canopyHeight"
  names(r_closure) <- "canopyClosure"

  return(list(
    canopyHeight = r_height,
    canopyClosure = r_closure
  ))
}


# predictClosureHeightFromCohort <- function(
#     cohortData,
#     pixelGroupMap,
#     modelHeight,
#     modelClosure
# ) {
#   require(data.table)
#   require(terra)
#   # optional: xgboost and caret only if needed by model class
#   # require(xgboost); require(caret)
#   
#   # --- prep ---
#   dt <- data.table::as.data.table(cohortData)
#   if ("pixelGroup" %in% names(dt) && !"pixelGroupID" %in% names(dt))
#     data.table::setnames(dt, "pixelGroup", "pixelGroupID")
#   
#   if (!all(c("pixelGroupID","age","B") %in% names(dt)))
#     stop("cohortData must contain pixelGroupID, age, and B")
#   
#   dt <- dt[!is.na(B) & B > 0]
#   dt[, B_raw := B]
#   dt[, c("age","B") := lapply(.SD, scale), .SDcols = c("age","B")]
#   
#   newdata_df <- as.data.frame(dt[, .(age, B)])
#   
#   get_expected_features <- function(model) {
#     if (inherits(model, "train")) {
#       setdiff(colnames(model$trainingData), ".outcome")
#     } else if (inherits(model, "xgb.Booster") && !is.null(model$feature_names)) {
#       model$feature_names
#     } else {
#       colnames(newdata_df) # fallback
#     }
#   }
#   
#   # align cols to model’s expected order (use intersection to be safe)
#   exp_h <- get_expected_features(modelHeight)
#   exp_c <- get_expected_features(modelClosure)
#   exp   <- unique(c(intersect(exp_h, colnames(newdata_df)),
#                     intersect(exp_c, colnames(newdata_df))))
#   if (length(exp) == 0L) exp <- colnames(newdata_df)
#   newdata_df <- newdata_df[, exp, drop = FALSE]
#   
#   predict_with_model <- function(model, df) {
#     if (inherits(model, "train")) {
#       predict(model, newdata = df)
#     } else if (inherits(model, "xgb.Booster")) {
#       m <- as.matrix(df); colnames(m) <- colnames(df)
#       dmat <- xgboost::xgb.DMatrix(data = m)
#       predict(model, dmat)
#     } else {
#       stop("Unsupported model class: ", paste(class(model), collapse = "/"))
#     }
#   }
#   
#   dt[, predicted_closure := as.numeric(predict_with_model(modelClosure, newdata_df))]
#   dt[, predicted_height  := as.numeric(predict_with_model(modelHeight,  newdata_df))]
#   
#   # --- aggregate to pixel ---
#   pix <- dt[, .(
#     height  = max(predicted_height, na.rm = TRUE),
#     closure = {
#       den <- sum(B_raw, na.rm = TRUE)
#       if (isTRUE(den == 0) || is.na(den)) NA_real_
#       else sum(predicted_closure * B_raw, na.rm = TRUE) / den
#     }
#   ), by = pixelGroupID]
#   
#   # --- rasterize by cell index ---
#   pg <- data.table::as.data.table(terra::as.data.frame(pixelGroupMap, cells = TRUE, na.rm = FALSE))
#   valcol <- setdiff(names(pg), "cell")[1]
#   data.table::setnames(pg, valcol, "pixelGroupID")
#   pg <- merge(pg, pix, by = "pixelGroupID", all.x = TRUE)
#   
#   r_tmpl <- terra::rast(pixelGroupMap)
#   r_hgt  <- r_tmpl; names(r_hgt)  <- "canopyHeight"
#   r_cls  <- r_tmpl; names(r_cls)  <- "canopyClosure"
#   r_hgt[pg$cell] <- pg$height
#   r_cls[pg$cell] <- pg$closure
#   
#   list(canopyHeight = r_hgt, canopyClosure = r_cls)
# }
