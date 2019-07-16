#' @title summariseSGS
#'
#' @description Summarise & Graph Redd/Carcass data by POP_NAME/SurveyYear
#'
#' @author Tyler Stright
#'
#' @examples summariseSGS()
#'
#' @import lubridate dplyr tidyr cdmsR
#' @export
#' @return NULL


summariseSGS <- function() {

  # Summarise Redd Data ---- 
tmp_reddsum <- getDatasetView(datastoreID = 78, cdms_host = cdms_host) %>%
  distinct(ActivityId, .keep_all = TRUE) %>%
  mutate(SpeciesRun = paste(Run, Species),
         SurveyYear = year(SurveyDate)) %>%
  group_by(SurveyYear, POP_NAME, SpeciesRun) %>%
  summarise(TotalRedds = sum(NewRedds, na.rm=TRUE))

  # Summarise Carcass Data ----
tmp_carcsum <- getDatasetView(datastoreID = 79, cdms_host = cdms_host) %>%
  mutate(SpeciesRun = paste(Run, Species),
         SurveyYear = year(SurveyDate),
         Origin = case_when(
            AdiposeFinClipped == 'No' & is.na(CWTCode) ~ "Natural",
            AdiposeFinClipped == 'No' & CWTCode == 'NA' ~ "Natural",
            AdiposeFinClipped == 'No' & !is.na(CWTCode) ~ "Hatchery",
            AdiposeFinClipped == 'Yes' ~ "Hatchery",
            is.na(AdiposeFinClipped) & is.na(CWTCode) ~ "Unknown",
            is.na(AdiposeFinClipped) & CWTCode == 'NA' ~ "Unknown",
            is.na(AdiposeFinClipped) & !is.na(CWTCode) ~ "Hatchery",
            AdiposeFinClipped == 'NA' & is.na(CWTCode) ~ "Unknown",
            AdiposeFinClipped == 'NA' & CWTCode == 'NA' ~ "Unknown",
            AdiposeFinClipped == 'NA' & !is.na(CWTCode) ~ "Hatchery",
            AdiposeFinClipped == 'Unknown' & is.na(CWTCode) ~ "Unknown",
            AdiposeFinClipped == 'Unknown' & CWTCode == 'NA' ~ "Unknown",
            AdiposeFinClipped == 'Unknown' & !is.na(CWTCode) ~ "Hatchery")
         ) %>%
  filter(!is.na(Count)) %>%
  select(POP_NAME, SurveyYear, SpeciesRun, Origin, Count, Sex, SpawnedOut)


# pHOS
phos_tmp <- tmp_carcsum %>%
  filter(Origin %in% c('Natural', 'Hatchery')) %>%
  group_by(POP_NAME, SurveyYear, SpeciesRun, Origin) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  spread(key = Origin, value = Count, fill = 0) %>%
  mutate(pHOS = round(100*(`Hatchery`/(`Hatchery` + `Natural`)), 2)) %>%
  select(-Hatchery, -Natural) 

# % Female
PF_tmp <- tmp_carcsum %>%
  filter(Sex %in% c('Male', 'Female')) %>%
  group_by(POP_NAME, SurveyYear, SpeciesRun, Sex) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  spread(key = Sex, value = Count, fill = 0) %>%
  mutate(`%Females` = round(100*(`Female`/(`Female` + `Male`)), 2)) %>%
  select(-Male)

# Prespawn Mortality
psm_tmp <- tmp_carcsum %>%
  filter(Sex == "Female") %>%
  mutate(PrespawnMort = case_when(
    SpawnedOut == 'Yes' ~ 'Spawned',
    SpawnedOut == 'No' ~ 'Prespawn Mortality',
    SpawnedOut == 'Unknown' ~ 'Unknown',
    SpawnedOut == 'NA' ~ "Unknown")
  ) %>%
  group_by(POP_NAME, SurveyYear, SpeciesRun, PrespawnMort) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  spread(key = PrespawnMort, value = Count, fill = 0) %>% 
  left_join(PF_tmp, by = c('POP_NAME', 'SurveyYear', 'SpeciesRun')) %>%
  mutate(PrespawnMortality = round(`Prespawn Mortality`/Female, 2)) %>%
  select(-Spawned, -Unknown, -Female, -`Prespawn Mortality`)

# Total Carcasses
all_carc <- tmp_carcsum %>%
  group_by(POP_NAME, SurveyYear, SpeciesRun) %>%
  summarise(TotalCarcass = sum(Count, na.rm = TRUE))

# Finalize SGS Summary Table (join redd/carcass tables from above and apply filters)
summary_df <- left_join(tmp_reddsum, phos_tmp, by = c('POP_NAME', 'SurveyYear', 'SpeciesRun')) %>%
    left_join(psm_tmp, by = c('POP_NAME', 'SurveyYear', 'SpeciesRun')) %>%
    left_join(all_carc, by = c('POP_NAME', 'SurveyYear', 'SpeciesRun')) %>%
    mutate(SurveyDate = paste0(SurveyYear, '-01-01')) %>% # this is needed to jive with Kus.
    select(SurveyYear, SurveyDate, POP_NAME, SpeciesRun,  TotalRedds, TotalCarcass, `%Females`, pHOS, PrespawnMortality)
  

# Graph Data -
# # total Redds / year
# sgs_sum1 <- plot_ly(data = summary_df, x= ~Year, y = ~`Total Redds`, type = 'scatter',
#                 mode = 'lines+markers', name = 'Total Redds')
# 
# # phos/%f/year
# sgs_sum2 <- plot_ly(data = summary_df, x= ~Year, y = ~`%Females`, type = 'scatter',
#                 mode = 'lines+markers', name = 'Percent Females') %>%
#   add_trace(y = ~`% Hatchery Spawners`, name = '% Hatchery Spawners', mode = 'lines+markers')
# 
# 
# sgs_sum3 <- subplot(sgs_sum1, sgs_sum2, nrows = 2, shareY = FALSE) %>%
#   layout(title = paste0('Total Redds, % Females, % Hatchery Spawners, by Year')) %>%
#   layout(legend = list(orientation = 'h', xanchor = 'center', x = 0.5, y = -0.15),
#          xaxis = list(title = 'Year'),
#          yaxis = list(title = 'Total Redds'),
#          xaxis2 = list(title = 'Year'),
#          yaxis2 = list(title = 'Percent'))

# sgs_redds <- ggplotly(ggplot(data = summary_df, aes(x= Year, y= `Total Redds`, colour = `Stream Name`)) +
#                    geom_point(size = 0.8, position = position_dodge(0.05)) +  
#                    geom_line(size = 0.5, position = position_dodge(0.05)) +
#                    theme_bw() +
#                    theme(legend.title = element_blank()) +
#                    theme(legend.position = 'bottom') +
#                    scale_y_continuous(labels = scales::comma) +
#                    scale_color_viridis_d() +
#                    labs(title = 'Total Redds, Percent Females, Percent Hatchery Origin Spawners (pHOS), and Prespawn Mortalities per Spawn Year',
#                          caption = 'some caption',
#                          x = 'Spawn Year',
#                          y = 'Total Redds',
#                          colour = 'Stream')) %>%
#   layout(legend = list(orientation = 'h', xanchor = 'center', x = 0.5, y = -0.2))
# 
# 
# sgs_fem <- ggplotly(ggplot(data = summary_df, aes(x= Year, y= `%Females`, colour = `Stream Name`)) +
#                    geom_point(size = 0.8, position = position_dodge(0.05)) +  
#                    geom_line(size = 0.5, position = position_dodge(0.05)) +
#                    theme_bw() +
#                    theme(legend.title = element_blank()) +
#                    theme(legend.position = 'bottom') +
#                    scale_y_continuous(labels = scales::comma) +
#                    scale_color_viridis_d() +
#                    labs(#title = '% Females',
#                        caption = 'some caption',
#                        x = '',
#                        y = '% Females',
#                        colour = 'Stream')) 
# 
# 
# 
# sgs_phos <- ggplotly(ggplot(data = summary_df, aes(x= Year, y= `% Hatchery Spawners`, colour = `Stream Name`)) +
#                    geom_point(size = 0.8, position = position_dodge(0.05)) +  
#                    geom_line(size = 0.5, position = position_dodge(0.05)) +
#                    theme_bw() +
#                    theme(legend.title = element_blank()) +
#                    theme(legend.position = 'bottom') +
#                    scale_y_continuous(labels = scales::comma) +
#                    scale_color_viridis_d() +
#                    labs(#title = '% Hatchery Spawners',
#                        caption = 'some caption',
#                        x = '',
#                        y = 'pHOS',
#                        colour = 'Stream')) 
# 
# 
# sgs_mort <- ggplotly(ggplot(data = summary_df, aes(x= Year, y= `Prespawn Mortality`, colour = `Stream Name`)) +
#                    geom_point(size = 0.8, position = position_dodge(0.05)) +  
#                    geom_line(size = 0.5, position = position_dodge(0.05)) +
#                    theme_bw() +
#                    theme(legend.title = element_blank()) +
#                    theme(legend.position = 'bottom') +
#                    scale_y_continuous(labels = scales::comma) +
#                    scale_color_viridis_d() +
#                    labs(#title = 'Prespawn Mortalities',
#                        caption = 'some caption',
#                        x = '',
#                        y = 'Prespawn Mortalities',
#                        colour = 'Stream')) 


# return(sgs_figs = list(summary_df, sgs_redds, sgs_fem, sgs_phos, sgs_mort))

  return(summary_df)
}
