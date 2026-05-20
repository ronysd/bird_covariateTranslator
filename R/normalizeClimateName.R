############################# normalizer climate variables name, theres missmatch between climate module variables and the pspclimate variables name


## Fourth Function
# normalizeClimateName <- function(x) {
#   
#   y <- x
#   
#   # remove spatial resolution suffixes
#   y <- sub("_(1km|5x5)$", "", y)
#   
#   # normalize degree-day names (DD0 to DD_0, DD18 to DD_18)
#   y <- sub("^DD([0-9]+)$", "DD_\\1", y)
#   
#   # normalize seasonal suffixes (wt, sm)
#   y <- sub("wt$", "_wt", y)
#   y <- sub("sm$", "_sm", y)
#   
#   # normalize common climate prefixes (ERA-style to canonical)
#   y <- sub("^ERATave", "Tave", y)
#   y <- sub("^ERAPPT", "PPT", y)
#   
#   return(y)
# }

normalizeClimateName <- function(x) {
  y <- x
  
  # remove resolution suffix
  y <- sub("_(1km|5x5)$", "", y)
  
  # annual degree-day variables only
  y <- sub("^DD_0$", "DD0", y)
  y <- sub("^DD_18$", "DD18", y)
  
  # seasonal variables only
  y <- sub("^Tave_wt$", "Tavewt", y)
  y <- sub("^Tave_sm$", "Tavesm", y)
  y <- sub("^PPT_wt$", "PPTwt", y)
  y <- sub("^PPT_sm$", "PPTsm", y)
  
  # monthly variables should NOT lose underscore/month suffix
  # DD5_02 must stay DD5_02
  # DD_0_11 must stay DD_0_11
  # PPT08, RH12, etc. stay as-is
  
  return(y)
}
