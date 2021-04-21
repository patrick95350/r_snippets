### Find Points in Polygons ####
#     Takes two data inputs:
#       1. A dataframe with XY (Long/Lat) data
#       2. Polygon shapefile
#     Converts the XY data into a spatial points data {sf} and identifies which
#       (if any) polygons the points fall inside of.
#     Exports a .csv file with the original data and the new labels
#        
#     By Patrick Rogers, California Research Bureau
#       April 2021
#
#     Uses the following Packages
#       sf
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

# Load Sourcefiles

# Custom Functions ####

match.xy.to.poly <- function(xy.data.file, poly.shape.file,
                             label.long = "longitude", label.lat = "latitude",
                             poly.label.field, poly.label.colname = "newvar"){
  # Function Parameters
  # xy.data.file        Filename of the XY data as a CSV, including location relative to working directory
  # label.long          Column name of the longitude variable in the XY data
  # label.lat           Column name of the latitude variable in the XY data
  # 
  # poly.shape.file     Filename of the POLY shapefile, including location relative to the working directory
  # poly.label.field    Column name of the label variable in the POLY data
  # poly.label.colname  Column name *to add* to the XY data for the new label column ####
  
  # Read in XY data, and convert it to sf object
  xy.data <- read.csv(file=xy.data.file, stringsAsFactors = FALSE)
  xy.data <- st_as_sf(x = xy.data, 
                      coords = c(label.long, label.lat))
  
  # Read in POLY data
  poly.shape <- st_read(dsn=poly.shape.file)
  
  # Set CRS for XY object to match POLY
  st_crs(xy.data) <- st_crs(poly.shape)
  
  # Get Overlaps
  overlap <- st_contains(poly.shape, xy.data, sparse = TRUE)
  
  # Use poly.label.field column to add labels to XY using poly.label.colname to name the new column
  xy.data[,poly.label.colname] <- NA
  
  for(i in 1:nrow(poly.shape)){
    xy.data[overlap[[i]],poly.label.colname] <- st_drop_geometry(poly.shape[,poly.label.field])[i,1]
  }
  
  return(xy.data)
}

# Example matching local library branches to Assembly and Senate Districts ####

branches_AD <- match.xy.to.poly(xy.data.file="Library Branches.csv",
                               poly.shape.file="tl_2020_06_sldl20\\tl_2020_06_sldl20.shp",
                               label.long="x", label.lat="y",
                               poly.label.field="SLDLST20", poly.label.colname="AD")

branches_SD <- match.xy.to.poly(xy.data.file="Library Branches.csv",
                                poly.shape.file="tl_2020_06_sldu20\\tl_2020_06_sldu20.shp",
                                label.long="x", label.lat="y",
                                poly.label.field="SLDUST20", poly.label.colname="SD")

branches_both <- branches_AD
branches_both$SD <- branches_SD$SD

write.csv(st_drop_geometry(branches_both),
          file = "Library Branches_Updated20210416_2.csv",
          row.names = FALSE)

# SCRAP ####
