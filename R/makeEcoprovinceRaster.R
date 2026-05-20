makeEcoprovinceRaster <- function(ecoregionVector, template, field = "ECOPROVINC") {
  library(sf)
  library(terra)
  
  eco <- sf::st_transform(ecoregionVector, terra::crs(template))
  eco[[field]] <- as.factor(eco[[field]])
  
  eco_vect <- terra::vect(eco)
  
  eco_rast <- terra::rasterize(
    eco_vect,
    template,
    field = field
  )
  
  return(eco_rast)
}
