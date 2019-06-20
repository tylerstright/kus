#' @title location_list
#'
#' @description filters CDMS locations to create Name/Latitude/Longitude dataframe
#' @description based on input criteria
#'
#' @param filter LocationTypeId of locations desired
#'
#' @author Tyler Stright
#'
#' @examples location_list(data = locations_df, locationtypeId = 1124)   # Retrieve RST Locations
#'
#' @import dplyr
#' @export
#' @return NULL

location_list <- function(data, locationtypeId) {
  
  # LocationTypeId : Description
  #-----------------------------
  # 1122 : SGS Transects
  # 1123 : Adult Weir Site (FINS Data)
  # 1124 : Rotary Screw Trap Site (P4/PTAGIS)
  # 1133 : In-Stream PIT Tag Array, 
  # 1134 : Age Data Sampling Locations
  # 1135 : Juvenile Survival 

  # partition CDMS getLocations output (Location/LocationType/WaterBody)
  waterbody_tbl <- data$WaterBody
  locationtype_tbl <- data$LocationType
  locations_tbl <- select(data, -LocationType, -WaterBody)
  
  tmp_loc <- locations_tbl %>%
    filter(LocationTypeId == locationtypeId) %>%
    select(Name, Latitude, Longitude)

   
  return(tmp_loc)
}