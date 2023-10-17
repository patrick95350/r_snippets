### Query US Census ArcGIS Rest API
#     This example queries the service providing Census Block geometries at:
#     https://tigerweb.geo.census.gov/arcgis/rest/services/TIGERweb/tigerWMS_Census2020/MapServer/10/
#
#     Notes:
#       Code could be adapted to query other ArcGIS  REST APIs fairly easily.
#       Query parameters follow SQL syntax
#
#     By Patrick Rogers, California Research Bureau
#       Oct 2023
#
#     Uses the following Packages
#       {here}
#       {sf}

# Clear the workspace
remove(list=ls(all=TRUE))

# Inits
# None

# Load Packages
library(here)
library(httr)
library(jsonlite)

# Load Sourcefiles
# None Used

# Custom Functions
getBlocks <- function(county, format, fields){
  # Define the URL of the Esri GeoREST server
  #url <- parse_url("https://tigerweb.geo.census.gov/arcgis/rest/services/Census2020/Tracts_Blocks/MapServer/2/query")
  url <- parse_url("https://tigerweb.geo.census.gov/arcgis/rest/services/TIGERweb/tigerWMS_Census2020/MapServer/10/query")
  
  # Define query parameters
  url$query <- list(
    where = paste0("STATE = '06' AND COUNTY = '", county, "'"),
    outFields = "STATE, COUNTY, TRACT, BLKGRP, BLOCK, POP100, UR", # Get FIPS codes as well as POP100 and UR fields
    f = "geojson" # Response format (GeoJSON)
  )
  
  # Create the API request
  request <- build_url(url)
  response <- GET(url = request)
  
  # Check the HTTP response status
  if (response$status_code == 200) {
    #cat(county, ": Query successful\n")
  } else {
    simpleError(paste0(county, ": Query failed with status code: ", response$status_code))
    # Handle errors or exit the script
  }
  
  json_response <- content(response, "text")
  
  if(!validate(json_response)){
    warning(paste0("Request for county ", county, " did not return valid JSON"))
    json_response <- NA
  }
  
  # Reset handle
  #handle_reset(request_h$url)
  
  return(json_response)
}

# User Parameters
# None Used

# Read in Data ####
# Get list of county FIPS
url <- parse_url("https://api.census.gov/data/2020/acs/acs5")
url$query <- list(get = "NAME",
                  `for` = "county:*",
                  `in` = "state:06")
request <- build_url(url)
response <- GET(url = request)
json_response <- content(response, "text")
counties <- data.frame(fromJSON(json_response)[-1,c(1,3)])

# Read in Block Data
counties_geojson <- vector(mode = "list", length = nrow(counties))
names(counties_geojson) <- gsub(" County, California", "", counties[,1])

progress_bar <- txtProgressBar(min=1,max=length(counties_geojson), style=3)

for(i in seq_along(counties_geojson)){
  counties_geojson[i] <- getBlocks(county = counties[i,2])
  setTxtProgressBar(progress_bar, i)
}

# EOF ####