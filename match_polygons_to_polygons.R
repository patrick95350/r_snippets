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

# Load Packages
library(sf)

# Load Sourcefiles

# Custom Functions ####

exclude <- function(first, second){
  # Drops any items from the data in the first parameter that also shows up in the second
  return(first[!(first %in% second)])
}

MatchPolyToPoly <- function(innerShape, outerShape,
                            innerShapeLabel, outerShapeLabel,
                            matchType = "wide"){
  # Function Parameters
  # innerShape        sf object for the smaller geographies that fit inside the larger polygons
  # innerShapeLabel   Column name if the ID variables for the innerShape. Needs to be one column
  # 
  # outerShape        sf object of the larger geographies, inside which the smaller fit
  # outerShapeLabel   Column name(s) if the ID variables for the outerShape. Can be multiple columns
  
  # matchType         Allows user to specify if the matching type
  #                     "wide" indicates it should be one row for each outerShape
  #                     "tall" indicates it should be one row for each innerShape/outerShape pair
  
  # Set CRS for XY object to match POLY
  if(st_crs(innerShape) != st_crs(outerShape)){
    print("Geographies do not match, transforming innerShape to match")
    innerShape <- st_transform(innerShape, st_crs(outerShape))
  }
  
  # Get Overlaps
  print("This might take a few minutes")
  tictoc::tic()
  intersects <- st_intersects(outerShape, innerShape, sparse = TRUE)
  touches <- st_touches(outerShape, innerShape, sparse = TRUE)
  tictoc::toc()
  
  overlaps <- Map(exclude, intersects, touches)
  
  # Construct crosswalk
  crosswalk <- NULL
  
  if(matchType == "tall"){
    # For tall crosswalks, do the following
    #   1.  Initialize the crosswalk
    #   2.  Loop over each row in the innerShape object:
    #       a.  Search across the overlaps list, checking for the current index value, 'i'
    #       b.  For each entry with that index present, add a new row to the crosswalk, with the labels for outerShape and innerShape
    
    tictoc::tic()
    crosswalk <- NULL
    
    for(i in 1:nrow(innerShape)){
      print(i)
      j <- sapply(overlaps, function (x) (length(grep(pattern = paste0("^", i, "$"), x = x)))==1)
      
      crosswalk <- rbind(crosswalk,
                         cbind(st_drop_geometry(outerShape[j,][outerShapeLabel]),
                               st_drop_geometry(innerShape[i,][innerShapeLabel]),
                               deparse.level = 0))
    }
    tictoc::toc()
    
  } else {
    crosswalk <- as.data.frame(cbind(st_drop_geometry(outerShape[outerShapeLabel]), NA))
    
    
    for(i in 1:length(overlaps)){
      # Get the ith row of outerShape, filter to the outerShapeLabel and drop the geometry
      # Get the ith entry in overlap and use that to select the correct innerShape rows, drop the geometry,
      #   and (for some reason) the results are in a n by 1 matrix, so need to select just the 1st column
      # Use paste with collapse to merge the tract IDs into one text string
      # Concatenate both into one vector, and rbind it to crosswalk
      crosswalk <- rbind(crosswalk,
                         c(st_drop_geometry(outerShape[i,outerShapeLabel]),
                           paste(st_drop_geometry(innerShape[overlaps[[i]],innerShapeLabel])[,1], collapse=", ")))
    }
    
    # Data.frame and add column names from outerShapeLabel and innerShapeLabel
    crosswalk <- data.frame(crosswalk)
    colnames(crosswalk) <- c(outerShapeLabel, innerShapeLabel)
    
    # For some reason, each of the columns are coming back as lists.
    for(i in 1:ncol(crosswalk)){
      if(is.list(crosswalk[,i])){
        crosswalk[,i] <- unlist(crosswalk[,i])
      }
    }
  }
  
  return(crosswalk)
}
# Example matching local library branches to Assembly and Senate Districts ####
                  
innerShape <- st_read(dsn=here("tl_2020_06_tract", "tl_2020_06_tract.shp"))
#colnames(innerShape)
innerShapeLabel = c("GEOID")

# Read in jurisdictions data
outerShape <- st_read(dsn=here("District_Boundaries", "Admin_Boundaries.shp"))
#colnames(outerShape)
outerShapeLabel = c("Name", "FSCS_KEY")

districts_tracts <- MatchPolyToPoly(innerShape = innerShape,
                                     outerShape = outerShape,
                                     innerShapeLabel = innerShapeLabel,
                                     outerShapeLabel = outerShapeLabel)

write.csv(districts_tracts,
          file = "Library Districts to Tracts_20210417_2.csv",
          row.names = FALSE)

# SCRAP ####
