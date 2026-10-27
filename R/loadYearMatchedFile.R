# loadYearMatchedFile <- function(folder, pattern, target_year, reader) {
#   files <- list.files(folder, pattern = pattern, full.names = TRUE)
#   if (length(files) == 0) stop("No matching files found in ", folder)
#   years <- as.integer(stringr::str_extract(files, "\\d{4}"))
#   idx <- if (all(is.na(years))) 1 else which.min(abs(years - target_year))
#   reader(files[idx])
# }

loadYearMatchedFile <- function(folder, pattern, target_year, reader) {
  
  files <- list.files(folder, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) stop("No matching files found in ", folder)
  
  file_names <- basename(files)
  years <- as.integer(stringr::str_extract(file_names, "\\d{4}"))
  
  if (all(is.na(years))) {
    idx <- 1
    matched_year <- NA_integer_
  } else {
    idx <- which.min(abs(years - target_year))
    matched_year <- years[idx]
  }
  
  # message(
  #   "loadYearMatchedFile | target_year = ", target_year,
  #   " | matched_year = ", matched_year,
  #   " | file = ", basename(files[idx])
  # )
  message(
    "loadYearMatchedFile | folder = ", basename(normalizePath(folder)),
    " | target_year = ", target_year,
    " | matched_year = ", matched_year,
    " | file = ", basename(files[idx])
  )  
  obj <- reader(files[idx])
  attr(obj, "source_file") <- files[idx]
  attr(obj, "source_year") <- matched_year
  attr(obj, "target_year") <- target_year
  
  obj
}