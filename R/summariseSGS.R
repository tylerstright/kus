#' @title summariseSGS:
#'
#' @description Summarise Redd/Carcass data by Stream/Year. Filter = Streams
#'
#' @param streamfilter input$summ_streams
#'
#' @author Tyler Stright
#'
#' @examples summariseSGS(streamfilter = input$summstreams, redd_data = redd_df, carcass_data = carc_df)
#'
#' @import lubridate dplyr? tidyr?
#' @export
#' @return NULL


summariseSGS <- function(streamfilter, redd_data, carcass_data) {
  
tmp_reddsum <- redd_data %>%
  distinct(ActivityId, .keep_all = TRUE) %>%
  separate(`SurveyDate`, into = 'SurveyDate', sep = "T") %>%
  mutate(`SurveyDate` = ymd(`SurveyDate`),
         `SurveyYear` = year(`SurveyDate`)) %>%
  group_by(StreamName, SurveyYear, TargetSpecies) %>% 
  summarise(TotalRedds = sum(NewRedds, na.rm = TRUE)) %>%
  ungroup() %>%
  select(StreamName, SurveyYear, TargetSpecies, TotalRedds)

tmp_carcsum <- carcass_data %>%
  separate(`SurveyDate`, into = 'SurveyDate', sep = "T") %>%
  mutate(`SurveyDate` = ymd(`SurveyDate`),
         `SurveyYear` = year(`SurveyDate`)) %>%
  select(StreamName, SurveyDate, SurveyYear, TargetSpecies, Count, Sex, SpawnedOut, PercentSpawned, AdiposeFinClipped, SnoutCollected, CWTCode) %>%
  # mutate(`AdiposeFinClipped` = case_when(
  #   `AdiposeFinClipped` %in%  c('NA', Unknown, NA) ~ 'No',   # ???? Need to deal with the odd AD values
  #   `AdiposeFinClipped` == 'No' ~ 'No',
  #   `AdiposeFinClipped` == 'Yes' ~ 'Yes'))
  mutate(Origin = ifelse(`SnoutCollected` == 'Yes', 'Hatchery',
                         ifelse(!is.na(`CWTCode`), 'Hatchery',
                                ifelse(`AdiposeFinClipped` == 'Yes', 'Hatchery', 'Natural'))))  # this is imperfect. There are inconsistencies in the data.

# %F
PF_tmp <- tmp_carcsum %>%
  filter(Sex %in% c('Male', 'Female')) %>%
  group_by(StreamName, SurveyYear, TargetSpecies, Sex) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  spread(key = Sex, value = Count, fill = 0) %>%
  mutate(`%Females` = round(100*(`Female`/(`Female` + `Male`)), 2))

# pHOS
phos_tmp <- tmp_carcsum %>%
  filter(Origin %in% c('Natural', 'Hatchery')) %>%
  group_by(StreamName, SurveyYear, TargetSpecies, Origin) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  spread(key = Origin, value = Count, fill = 0) %>%
  mutate(pHOS = round(100*(`Hatchery`/(`Hatchery` + `Natural`)), 2))

# Prespawn Mortality
psm_tmp <- tmp_carcsum %>%
  filter(Sex == "Female") %>%
  mutate(`PrespawnMort` = case_when(
    `SpawnedOut` == 'Yes' ~ 'No',
    `SpawnedOut` == "No" ~ 'Prespawn Mortality',
    `PercentSpawned` == -99 ~ 'Unknown',  # Lots of Unknowns.  May want to try and improve this.
    `PercentSpawned` <= 25 ~ 'Prespawn Mortality' 
  )) %>%
  group_by(StreamName, SurveyYear, TargetSpecies, PrespawnMort ) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  spread(key = `PrespawnMort`, value = Count, fill = 0) %>%
  select(-'<NA>', -No, -Unknown)

# Total Carcasses
all_carc <- tmp_carcsum %>%
  group_by(StreamName, SurveyYear, TargetSpecies) %>%
  summarise(`Carcass Total` = sum(Count, na.rm = TRUE))

# Finalize SGS Summary Table (join redd/carcass tables from above and apply filters)
summary_df <- left_join(tmp_reddsum, PF_tmp, by = c('StreamName', 'SurveyYear', 'TargetSpecies')) %>%
    left_join(phos_tmp, by = c('StreamName', 'SurveyYear', 'TargetSpecies')) %>%
    left_join(psm_tmp, by = c('StreamName', 'SurveyYear', 'TargetSpecies')) %>%
    left_join(all_carc, by = c('StreamName', 'SurveyYear', 'TargetSpecies')) %>%
    filter(StreamName %in% streamfilter) %>% 
    mutate(`Species` = case_when(
      `TargetSpecies` == 'F_CHN' ~ 'Fall Chinook',
      `TargetSpecies` == 'S_CHN' ~ 'Spring/Summer Chinook',
      `TargetSpecies` == 'S_STH' ~ 'Summer Steelhead',
      `TargetSpecies` == 'BT' ~ 'Bull Trout'
    )) %>%
    select(StreamName, SurveyYear, Species,  everything(), -TargetSpecies) %>%
    rename('Stream Name' = StreamName, 'Year' = SurveyYear, 'Total Redds' = TotalRedds,
           'Hatchery Origin' = Hatchery, 'Natural Origin' = Natural, '% Hatchery Spawners' = pHOS)
  
return(summary_df)

} 
