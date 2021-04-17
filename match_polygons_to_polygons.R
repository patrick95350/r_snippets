### Match Polygons to Larger Polygons
#     Takes 2 data inputs
#       1. A shapefile with smaller polygons
#       2. A shapefile with larger polygons
#
#     By Patrick Rogers, California Research Bureau
#       April 2021
#
#     Uses the following Packages
#       {sf}
#
#     Uses the following data
#       Shapefile with polygons
#       Shapefile with polygons

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

exclude <- function(first, second){
  # Drops any items from the data in the first parameter that also shows up in the second
  return(first[!(first %in% second)])
}

match.poly.to.poly <- function(lower.shape.file, upper.shape.file,
                               lower.shape.label, upper.shape.label){
  # Function Parameters
  # lower.shape.file    Filename of the more granular POLY shapefile, including location relative to working directory
  # lower.shape.label   Column name if the ID variables for the lower.shape. Needs to be one column
  # 
  # poly.shape.file     Filename of the less granular POLY shapefile, including location relative to the working directory
  # poly.label.label    Column name(s) if the ID variables for the upper.shape. Can be multiple columns

  # Read in tract data
  lower.shape <- st_read(dsn=lower.shape.file)

  # Read in POLY data
  upper.shape <- st_read(dsn=upper.shape.file)
  
  # Set CRS for XY object to match POLY
  if(st_crs(lower.shape) != st_crs(upper.shape)){
    lower.shape <- st_transform(lower.shape, st_crs(upper.shape))
  }
  
  # Get Overlaps
  print("This might take a few minutes")
  tictoc::tic()
  intersects <- st_intersects(upper.shape, lower.shape, sparse = TRUE)
  touches <- st_touches(upper.shape, lower.shape, sparse = TRUE)
  tictoc::toc()
  
  overlaps <- Map(exclude, intersects, touches)

  # Construct crosswalk
  crosswalk <- NULL
  
  for(i in 1:length(overlaps)){
    # Get the ith row of upper.shape, filter to the upper.shape.label and drop the geometry
    # Get the ith entry in overlap and use that to select the correct lower.shape rows, drop the geometry,
    #   and (for some reason) the results are in a n by 1 matrix, so need to select just the 1st column
    # Use paste with collapse to merge the tract IDs into one text string
    # Concatenate both into one vector, and rbind it to crosswalk
    crosswalk <- rbind(crosswalk,
                       c(st_drop_geometry(upper.shape[i,upper.shape.label]),
                         paste(st_drop_geometry(lower.shape[overlaps[[i]],lower.shape.label])[,1], collapse=", ")))
  }
  
  # Data.frame and add column names from upper.shape.label and lower.shape.label
  crosswalk <- data.frame(crosswalk)
  colnames(crosswalk) <- c(upper.shape.label, lower.shape.label)
  
  # For some reason, each of the columns are coming back as lists.
  for(i in 1:ncol(crosswalk)){
    if(is.list(crosswalk[,i])){
      crosswalk[,i] <- unlist(crosswalk[,i])
    }
  }
  
  return(crosswalk)
}

# Example matching local library branches to Assembly and Senate Districts ####

districts_tracts <- match.poly.to.poly(lower.shape.file = "tl_2020_06_tract\\tl_2020_06_tract.shp",
                                       upper.shape.file = "District_Boundaries\\Admin_Boundaries.shp",
                                       lower.shape.label = "GEOID",
                                       upper.shape.label = c("Name", "FSCS_KEY"))

write.csv(districts_tracts,
          file = "Library Districts to Tracts_20210417_2.csv",
          row.names = FALSE)

# SCRAP ####