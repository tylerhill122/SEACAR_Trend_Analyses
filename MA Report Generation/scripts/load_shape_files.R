library(sf)
# AP and NERR shapefiles
source("scripts/SEACAR_data_location.R")
shape_files <- list.files(seacar_shape_location, full=TRUE)

shape_locate <- function(location){return(paste0(seacar_shape_location, location))}

AP_shp <- st_read(shape_locate("APs/Florida_Aquatic_Preserves.shp"))
NERR_shp <- st_read(shape_locate("NERRs/Florida_National_Estuarine_Resarch_Reserves__NERR__Boundaries.shp"))

GeoDBdate <- "12dec2023"
locs_pts <- st_read(shape_locate(paste0("SampleLocations", GeoDBdate, "/seacar_dbo_vw_SampleLocation_Point.shp")))
locs_lns <- st_read(shape_locate(paste0("SampleLocations", GeoDBdate, "/seacar_dbo_vw_SampleLocation_Line.shp")))
rcp <- st_read(shape_locate("orcp_all_sites/ORCP_Managed_Areas.shp"))
counties <- st_read(shape_locate("FLCounties/Counties_-_Detailed_Shoreline.shp"))
corners <- fread(shape_locate("MApolygons_corners.csv"))

###############
## FUNCTIONS ##
###############

# Allows location of shapefile for each MA
find_shape <- function(ma){
  if (grepl("National Estuarine", ma, fixed = TRUE)){
    shape_file <- NERR_shp %>% filter(SITE_NAME==ma)
  } else if (grepl("Aquatic Preserve", ma, fixed = TRUE)) {
    shape_file <- AP_shp %>% filter(LONG_NAME==ma)
  }
  return(shape_file)
}

# Gets coordinate min and max from shapefile
# This allows for accurately setting view on the map
get_shape_coordinates <- function(ma_shape){
  bbox_list <- lapply(st_geometry(ma_shape), st_bbox)
  maxmin <- as.data.frame(matrix(unlist(bbox_list),nrow=nrow(ma_shape)))
  names(maxmin) <- names(bbox_list[[1]])
  return(maxmin)
}