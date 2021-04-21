### Match Polygon to Nearest Point with Distance ####
#     Takes two data inputs:
#       1. A dataframe with XY (Long/Lat) data
#       2. Polygon shapefile
#     Converts the XY data into a spatial points data {sf} and identifies which
#       point is closest to the centroid of each polygon
#
#     By Patrick Rogers, California Research Bureau
#       April 2021
#
#     Uses the following Packages
#       {sf}
#       {lwgeom}
#
#     Uses the following data
#       Data frame with XY data
#       shapefile with polygons

# Clear the workspace ####
remove(list=ls(all=TRUE))

# Inits
rdrive.dir <- "R:\\CRB CURRENT REQUESTS\\FILENAMEHERE"
local.dir <- "C:\\Users\\patri\\Desktop\\CSL Data"

if (file.exists(rdrive.dir)){
  setwd(rdrive.dir)
} else {
  setwd(local.dir)
}

# Load Packages
library(sf)
library(lwgeom)

# Load Sourcefiles

# User Parameters
xy.data.file <- "Library Branches.csv"
poly.shape.file <- "C:\\Users\\patri\\Desktop\\Redlining\\Maps\\Unzipped\\tl_2020_06_tabblock10\\tl_2020_06_tabblock10.shp"
label.long <- "x"
label.lat <- "y"
xy.data.label <- c("FSCS_KEY", "FSCS_SEQ")

# Custom Functions ####

dist.poly.to.xy <- function(xy.data.file, poly.shape.file,
                            label.long = "longitude", label.lat = "latitude"){
  # Takes an XY data file and a POLY shapefile, and calculates the distance of
  #   each POLY object from it's centroid to the nearest XY point object.
  # The ID columns of the XY points are transferred to the dataframe of the
  #   POLY object, as well as the distance, in meters.
  
  # Function Parameters
  # xy.data.file      Filename of the XY data as a CSV, including location
  #   relative to working directory
  # poly.shape.file   Filename of the POLY shapefile, including location
  #   relative to the working directory
  # label.long        Column name of the longitude variable in the XY data
  # label.lat         Column name of the latitude variable in the XY data
  # xy.data.label     Column name of the label variable in the XY data.
  #                     *Also added to the POLY output to identify nearest XY point
  
  # Read in XY data as a CSV
  xy.data <- read.csv(file=xy.data.file, stringsAsFactors = FALSE)
  xy.data <- st_as_sf(x = xy.data, 
                      coords = c(label.long, label.lat))
  
  # If the CSV is encoded UTF-8, it injects junk text into the 1st column name
  colnames(xy.data) <- gsub(pattern = "ï..", "", colnames(xy.data))
  
  # Read in POLY data
  poly.shape <- st_read(dsn=poly.shape.file)
  
  # Set CRS for XY object to match POLY
  if(st_crs(xy.data) != st_crs(poly.shape)){
    st_crs(xy.data) <- st_crs(poly.shape)
    xy.data <- st_transform(xy.data, st_crs(poly.shape))
  }
  
  # Identify the nearest XY point for each poly
  nearest <- st_nearest_feature(poly.shape, xy.data)
  
  # Get the POLY centroids
  poly.center <- st_centroid(poly.shape)
  
  # Calculate the distance of each POLY from it's nearest XY point
  print("This might take a few minutes.")
  tictoc::tic()
  distance <- st_distance(poly.center, xy.data[nearest,], by_element = TRUE)
  tictoc::toc()
  
  # Append the XY labels and distance measures to poly.shape
  poly.shape[,xy.data.label] <- xy.data[nearest,xy.data.label]
  poly.shape$DIST <- distance
  
  return(poly.shape)
}

# Example calculating distance of census blocks to local library branches ####

blocks_to_libs <- dist.poly.to.xy(xy.data.file = "Library Branches.csv",
                                  poly.shape.file = "C:\\Users\\patri\\Desktop\\Redlining\\Maps\\Unzipped\\tl_2020_06_tabblock10\\tl_2020_06_tabblock10.shp",
                                  label.long = "x", label.lat = "y",
                                  xy.data.label = c("FSCS_KEY", "FSCS_SEQ"))

write.csv(st_drop_geometry(blocks_to_libs),
          file = "CA Census Block Distance to Libraries.csv",
          row.names = FALSE)

st_write(blocks_to_libs,
         dsn = "Block_to_Library_Distance.shp",
         layer = "Distances",
         append=FALSE)
         
# Step 1 ####

# Step 2 ####

# Step 3 ####
