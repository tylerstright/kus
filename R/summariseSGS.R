#' @title summariseSGS
#'
#' @description Summarise & Graph Redd/Carcass data by POP_NAME/Year
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
         Year = year(SurveyDate)) %>%
  group_by(Year, POP_NAME, SpeciesRun) %>%
  summarise(TotalRedds = sum(NewRedds, na.rm=TRUE)) %>%
  ungroup()

  # Summarise Carcass Data ----
tmp_carcsum <- getDatasetView(datastoreID = 79, cdms_host = cdms_host) %>%
  mutate(SpeciesRun = paste(Run, Species),
         Year = year(SurveyDate),
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
  select(POP_NAME, Year, SpeciesRun, Origin, Count, Sex, SpawnedOut)


# pHOS
phos_tmp <- tmp_carcsum %>%
  filter(Origin %in% c('Natural', 'Hatchery')) %>%
  group_by(POP_NAME, Year, SpeciesRun, Origin) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  spread(key = Origin, value = Count, fill = 0) %>%
  mutate(pHOS = round((`Hatchery`/(`Hatchery` + `Natural`)), 2)) %>%
  select(-Hatchery, -Natural)%>%
  ungroup()

# % Female
PF_tmp <- tmp_carcsum %>%
  filter(Sex %in% c('Male', 'Female')) %>%
  group_by(POP_NAME, Year, SpeciesRun, Sex) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  spread(key = Sex, value = Count, fill = 0) %>%
  mutate(`%Females` = round((`Female`/(`Female` + `Male`)), 2)) %>%
  select(-Male)%>%
  ungroup()

# Prespawn Mortality
psm_tmp <- tmp_carcsum %>%
  filter(Sex == "Female") %>%
  mutate(PrespawnMort = case_when(
    SpawnedOut == 'Yes' ~ 'Spawned',
    SpawnedOut == 'No' ~ 'Prespawn Mortality',
    SpawnedOut == 'Unknown' ~ 'Unknown',
    SpawnedOut == 'NA' ~ "Unknown")
  ) %>%
  group_by(POP_NAME, Year, SpeciesRun, PrespawnMort) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  spread(key = PrespawnMort, value = Count, fill = 0) %>% 
  left_join(PF_tmp, by = c('POP_NAME', 'Year', 'SpeciesRun')) %>%
  mutate(PrespawnMortality = round(`Prespawn Mortality`/Female, 2)) %>%
  select(-Spawned, -Unknown, -Female, -`Prespawn Mortality`)%>%
  ungroup()

# Total Carcasses
all_carc <- tmp_carcsum %>%
  group_by(POP_NAME, Year, SpeciesRun) %>%
  summarise(TotalCarcass = sum(Count, na.rm = TRUE))%>%
  ungroup()

# Finalize SGS Summary Table (join redd/carcass tables from above and apply filters)
summary_df <- left_join(tmp_reddsum, phos_tmp, by = c('POP_NAME', 'Year', 'SpeciesRun')) %>%
    left_join(psm_tmp, by = c('POP_NAME', 'Year', 'SpeciesRun')) %>%
    left_join(all_carc, by = c('POP_NAME', 'Year', 'SpeciesRun')) %>%
    select(Year, POP_NAME, SpeciesRun,  TotalRedds, TotalCarcass, `%Females`, pHOS, PrespawnMortality)
  

# Graph Data ----
# total Redds / year
# sgs_sum1 <- plot_ly(data = summary_df, 
#                     x= ~Year, 
#                     y = ~TotalRedds, 
#                     type = 'scatter',
#                     mode = 'lines+markers',
#                     line = list(color = viridis_pal(option="D")(length(unique(summary_df$POP_NAME)))),
#                     name = ~POP_NAME,
#                     color = ~POP_NAME,
#                     colors = viridis_pal(option="D")(length(unique(summary_df$POP_NAME))),
#                     # legendgroup = ~POP_NAME,
#                     showlegend = TRUE)
# 
# print(sgs_sum1)
# 
# # phos/%f/year
# sgs_sum2 <- plot_ly(data = summary_df, x= ~Year, y = ~`%Females`, type = 'scatter',
#                 mode = 'lines+markers', 
#                 name = ~POP_NAME,
#                 color = ~POP_NAME,
#                 colors = viridis_pal(option="D")(length(unique(summary_df$POP_NAME))),
#                 legendgroup = ~POP_NAME) %>%
#   add_trace(y = ~`pHOS`, name = ~ POP_NAME, mode = 'lines+markers', showlegend = FALSE)
# 
# print(sgs_sum2)
# 
# 
# sgs_sum3 <- subplot(sgs_sum1, sgs_sum2, nrows = 2, shareY = FALSE) %>%
#   layout(title = paste0('Total Redds, % Females, % Hatchery Spawners, by Year')) %>%
#   layout(legend = list(orientation = 'h', xanchor = 'center', x = 0.5, y = -0.15),
#          xaxis = list(title = 'Year'),
#          yaxis = list(title = 'Total Redds'),
#          xaxis2 = list(title = 'Year'),
#          yaxis2 = list(title = 'Percent'))
# 
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

# Return ----

# return(sgs_figs = list(summary_df, sgs_redds, sgs_fem, sgs_phos, sgs_mort))

  return(summary_df)
}
