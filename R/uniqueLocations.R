uniqueLocations <- function(data = locations_df){
  
  # partition CDMS getLocations output (Location/LocationType/WaterBody)
  waterbody_tbl <- data$WaterBody
  locationtype_tbl <- data$LocationType
  locations_tbl <- select(data, -LocationType, -WaterBody)
  
  
  tmp <- locations_tbl %>%
    mutate(type = case_when(
      # RST
      grepl('Rotary Screw Trap', Name) & LocationTypeId == 1124 ~ 'Rotary Screw Trap - P4',

      # SGS
      grepl('Transect', Name) & LocationTypeId == 1122 ~ 'SGS Transect',
      grepl('Transect Type', Description) & LocationTypeId == 1122 ~ 'SGS Transect',
      
      # Age Data
      grepl('Transect Type', Description) & LocationTypeId == 1134 ~ 'Age - SGS',
      grepl('Transect', Name) & LocationTypeId == 1134 ~ 'Age- SGS', 
      grepl('Trap', Name) & LocationTypeId == 1134 ~ 'Age - Adult Trap',
      grepl('Weir', Name) & LocationTypeId == 1134 ~ 'Age - Adult Weir',
      grepl('Tag Array', Description) & LocationTypeId == 1134 ~ 'Age - PIT Array',   
      
      # Weirs
      grepl('Weir', Name) & LocationTypeId == 1123 ~ 'Adult Weir - FINS',
      grepl('Trap', Name) & LocationTypeId == 1123 ~ 'Adult Trap - FINS',
      
      # PIT Tag Arrays
      grepl('Tag Array', Description) & LocationTypeId == 1133 ~ 'PIT Array', 
      
      # Survival Data
      grepl('Rotary Screw Trap', Name) & LocationTypeId == 1135 ~ 'Survival - Rotary Screw Trap',
      grepl('Acclimation', Name) ~ 'Survival - Acclimation Site',
      grepl('Release Site', Name) ~ 'Survival - Release Site',
      LocationTypeId == 1135 ~ 'Survival - ?' 
    )) 
  
  return(tmp)
}

