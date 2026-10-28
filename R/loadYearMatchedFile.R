# loadYearMatchedFile <- function(folder, pattern, target_year, reader) {
#   files <- list.files(folder, pattern = pattern, full.names = TRUE)
#   if (length(files) == 0) stop("No matching files found in ", folder)
#   years <- as.integer(stringr::str_extract(files, "\\d{4}"))
#   idx <- if (all(is.na(years))) 1 else which.min(abs(years - target_year))
#   reader(files[idx])
# }

# loadYearMatchedFile <- function(folder, pattern, target_year, reader) {
  
#   files <- list.files(folder, pattern = pattern, full.names = TRUE)
#   if (length(files) == 0) stop("No matching files found in ", folder)
  
#   file_names <- basename(files)
#   years <- as.integer(stringr::str_extract(file_names, "\\d{4}"))
  
#   if (all(is.na(years))) {
#     idx <- 1
#     matched_year <- NA_integer_
#   } else {
#     idx <- which.min(abs(years - target_year))
#     matched_year <- years[idx]
#   }
  
#   # message(
#   #   "loadYearMatchedFile | target_year = ", target_year,
#   #   " | matched_year = ", matched_year,
#   #   " | file = ", basename(files[idx])
#   # )
#   message(
#     "loadYearMatchedFile | folder = ", basename(normalizePath(folder)),
#     " | target_year = ", target_year,
#     " | matched_year = ", matched_year,
#     " | file = ", basename(files[idx])
#   )  
#   obj <- reader(files[idx])
#   attr(obj, "source_file") <- files[idx]
#   attr(obj, "source_year") <- matched_year
#   attr(obj, "target_year") <- target_year
  
#   obj
# }
loadYearMatchedFile <- function(folder, pattern, target_year, reader) {
  if (!dir.exists(folder)) stop("Snapshot folder does not exist: ", folder)
  files <- list.files(folder, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) stop("No files matching '", pattern, "' in ", folder)

  # exactly one standalone 4-digit token is required in each file name
  tokens <- stringr::str_extract_all(basename(files), "(?<![0-9])[0-9]{4}(?![0-9])")
  nTok <- lengths(tokens)
  if (any(nTok != 1L)) {
    stop("Cannot parse a single unambiguous year from: ",
         paste(basename(files)[nTok != 1L], collapse = ", "))
  }
  years <- as.integer(unlist(tokens))

  idx <- which(years == as.integer(target_year))
  if (length(idx) == 0) {
    stop("No snapshot for year ", target_year, " in ", folder,
         ". Available years: ", paste(sort(unique(years)), collapse = ", "))
  }
  if (length(idx) > 1) {
    stop("Multiple snapshots for year ", target_year, " in ", folder, ": ",
         paste(basename(files)[idx], collapse = ", "))
  }

  message("loadYearMatchedFile | ", basename(normalizePath(folder)),
          " | year = ", target_year, " | file = ", basename(files[idx]))
  obj <- reader(files[idx])
  attr(obj, "source_file") <- files[idx]
  attr(obj, "source_year") <- years[idx]
  attr(obj, "target_year") <- target_year
  obj
}