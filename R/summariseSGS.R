#' @title summariseSGS
#'
#' @description Summarise & Graph Redd/Carcass data by POP_NAME/Year for Kus
#'
#' @author Tyler Stright
#'
#' @examples summariseSGS()
#'
#' @import tidyverse cdmsR cuyem
#' @export
#' @return NULL


summariseSGS <- function(redd_data, carcass_data) {

  # Total Redds
  total_redds <- redd_data %>%
    distinct(ActivityId, .keep_all = TRUE) %>%
    group_by(Year, SpeciesRun, POP_NAME) %>%
    summarize(TotalRedds = sum(NewRedds, na.rm = TRUE))
  
  # Total Carcass
  total_carcass <- carcass_data %>%
    group_by(Year, SpeciesRun, POP_NAME) %>%
    summarize(TotalCarcass = sum(Count, na.rm = TRUE))
  
  # pHOS
  c_phos <- carcass_data %>%
    filter(CarcassSpecies %in% c('F_CHN', 'S_CHN', 'S_STH')) %>% 
    est_group_p(.summary_var = Origin, alpha = 0.05, Year, POP_NAME, SpeciesRun) %>%
    filter(Origin == 'Hatchery') %>%
    mutate(p = round(p, 2))
  
  names(c_phos)[5:9] <- paste('phos_', names(c_phos)[5:9], sep='')
  
  # Percent Female
  c_pfem <- carcass_data %>%
    filter(CarcassSpecies %in% c('F_CHN', 'S_CHN', 'S_STH'),
           Sex %in% c('Male', 'Female')) %>%
    est_group_p(.summary_var = Sex, alpha = 0.05, Year, POP_NAME, SpeciesRun) %>%
    filter(Sex == 'Female') %>%
    mutate(p = round(p, 2))
  
  names(c_pfem)[5:9] <- paste('pfem_', names(c_pfem)[5:9], sep='')
  
  #Prespawn Mortality
  c_pmort <- carcass_data %>%
    mutate(PrespawnMort = case_when(
      SpawnedOut == 'Yes' ~ 'Spawned',
      SpawnedOut == 'No' ~ 'Prespawn Mortality',
      SpawnedOut == 'Unknown' ~ 'Unknown',
      SpawnedOut == 'NA' ~ "Unknown")
    ) %>%
    filter(CarcassSpecies %in% c('F_CHN', 'S_CHN', 'S_STH'),
           Sex == 'Female',
           PrespawnMort != 'Unknown') %>%
    est_group_p(.summary_var = PrespawnMort, alpha = 0.05, Year, POP_NAME, SpeciesRun) %>%
    filter(PrespawnMort == 'Prespawn Mortality') %>%
    mutate(p = round(p, 2))
  
  names(c_pmort)[5:9] <- paste('pmort_', names(c_pmort)[5:9], sep='')
  
  # JOIN
  sgs_summary_df <- full_join(total_redds, total_carcass, by = c('Year', 'POP_NAME', 'SpeciesRun')) %>%
    full_join(c_phos, by = c('Year', 'POP_NAME', 'SpeciesRun')) %>%
    full_join(c_pfem, by = c('Year', 'POP_NAME', 'SpeciesRun')) %>%
    full_join(c_pmort, by = c('Year', 'POP_NAME', 'SpeciesRun')) %>%
    select(Year, POP_NAME, SpeciesRun, TotalRedds, TotalCarcass, pfem_p, phos_p, pmort_p)
  
#   # Summarise Redd Data ---- 
# tmp_reddsum <- getDatasetView(datastoreID = 78, cdms_host = cdms_host) %>%
#   distinct(ActivityId, .keep_all = TRUE) %>%
#   mutate(SpeciesRun = paste(Run, Species)) %>% 
#   group_by(Year, POP_NAME, SpeciesRun) %>%
#   summarise(TotalRedds = sum(NewRedds, na.rm=TRUE)) %>%
#   ungroup()
# 
#   # Summarise Carcass Data ----
# tmp_carcsum <- getDatasetView(datastoreID = 79, cdms_host = cdms_host) %>%
#   mutate(SpeciesRun = paste(Run, Species),
#          Origin = case_when(
#             AdiposeFinClipped == 'No' & is.na(CWTCode) ~ "Natural",
#             AdiposeFinClipped == 'No' & CWTCode == 'NA' ~ "Natural",
#             AdiposeFinClipped == 'No' & !is.na(CWTCode) ~ "Hatchery",
#             AdiposeFinClipped == 'Yes' ~ "Hatchery",
#             is.na(AdiposeFinClipped) & is.na(CWTCode) ~ "Unknown",
#             is.na(AdiposeFinClipped) & CWTCode == 'NA' ~ "Unknown",
#             is.na(AdiposeFinClipped) & !is.na(CWTCode) ~ "Hatchery",
#             AdiposeFinClipped == 'NA' & is.na(CWTCode) ~ "Unknown",
#             AdiposeFinClipped == 'NA' & CWTCode == 'NA' ~ "Unknown",
#             AdiposeFinClipped == 'NA' & !is.na(CWTCode) ~ "Hatchery",
#             AdiposeFinClipped == 'Unknown' & is.na(CWTCode) ~ "Unknown",
#             AdiposeFinClipped == 'Unknown' & CWTCode == 'NA' ~ "Unknown",
#             AdiposeFinClipped == 'Unknown' & !is.na(CWTCode) ~ "Hatchery")
#          ) %>%
#   filter(!is.na(Count)) %>%
#   select(POP_NAME, Year, SpeciesRun, Origin, Count, Sex, SpawnedOut)
# 
# 
# # pHOS
# phos_tmp <- tmp_carcsum %>%
#   filter(Origin %in% c('Natural', 'Hatchery')) %>%
#   group_by(POP_NAME, Year, SpeciesRun, Origin) %>%
#   summarise(Count = sum(Count, na.rm = TRUE)) %>%
#   spread(key = Origin, value = Count, fill = 0) %>%
#   mutate(pHOS = round((`Hatchery`/(`Hatchery` + `Natural`)), 2)) %>%
#   select(-Hatchery, -Natural)%>%
#   ungroup()
# 
# # % Female
# PF_tmp <- tmp_carcsum %>%
#   filter(Sex %in% c('Male', 'Female')) %>%
#   group_by(POP_NAME, Year, SpeciesRun, Sex) %>%
#   summarise(Count = sum(Count, na.rm = TRUE)) %>%
#   spread(key = Sex, value = Count, fill = 0) %>%
#   mutate(PercentFemales = round((`Female`/(`Female` + `Male`)), 2)) %>%
#   select(-Male)%>%
#   ungroup()
# 
# # Prespawn Mortality
# psm_tmp <- tmp_carcsum %>%
#   filter(Sex == "Female") %>%
#   mutate(PrespawnMort = case_when(
#     SpawnedOut == 'Yes' ~ 'Spawned',
#     SpawnedOut == 'No' ~ 'Prespawn Mortality',
#     SpawnedOut == 'Unknown' ~ 'Unknown',
#     SpawnedOut == 'NA' ~ "Unknown")
#   ) %>%
#   group_by(POP_NAME, Year, SpeciesRun, PrespawnMort) %>%
#   summarise(Count = sum(Count, na.rm = TRUE)) %>%
#   spread(key = PrespawnMort, value = Count, fill = 0) %>% 
#   left_join(PF_tmp, by = c('POP_NAME', 'Year', 'SpeciesRun')) %>%
#   mutate(PrespawnMortality = round(`Prespawn Mortality`/Female, 2)) %>%
#   select(-Spawned, -Unknown, -Female, -`Prespawn Mortality`)%>%
#   ungroup()
# 
# # Total Carcasses
# all_carc <- tmp_carcsum %>%
#   group_by(POP_NAME, Year, SpeciesRun) %>%
#   summarise(TotalCarcass = sum(Count, na.rm = TRUE))%>%
#   ungroup()
# 
# # Finalize SGS Summary Table (join redd/carcass tables from above and apply filters)
# summary_df <- left_join(tmp_reddsum, phos_tmp, by = c('POP_NAME', 'Year', 'SpeciesRun')) %>%
#     left_join(psm_tmp, by = c('POP_NAME', 'Year', 'SpeciesRun')) %>%
#     left_join(all_carc, by = c('POP_NAME', 'Year', 'SpeciesRun')) %>%
#     select(Year, POP_NAME, SpeciesRun,  TotalRedds, TotalCarcass, `PercentFemales`, pHOS, PrespawnMortality)
#   



  return(sgs_summary_df)
}
