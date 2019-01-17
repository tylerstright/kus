#' @title location_list
#'
#' @description filters CDMS locations to create Name/Latitude/Longitude dataframe
#' @description based on input criteria
#'
#' @param filter LocationTypeId of locations desired
#'
#' @author Tyler Stright
#'
#' @examples location_list(filter = 1124)   # Retrieve RST Locations
#'
#' @import dplyr
#' @export
#' @return NULL

location_list <- function(LocationTypeId) {
  
  # SGS Transects: 1122
  # Weirs : 1123
  # Rotary Screw Traps: 1124
  
  # partition CDMS getLocations output (Location/LocationType/WaterBody)
  waterbody_tbl <- locations_df$WaterBody
  locationtype_tbl <- locations_df$LocationType
  locations_tbl <- select(locations_df, -LocationType, -WaterBody)
  
  tmp_loc <- locations_tbl %>%
    filter(LocationTypeId == LocationTypeId) %>%
    select(Name, Latitude, Longitude)

   
  return(tmp_loc)
}