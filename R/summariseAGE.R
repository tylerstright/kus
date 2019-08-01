#' @title summariseAGE
#'
#' @description Summarise & Graph AGE data by POPULATION
#'
#' @author Tyler Stright
#'
#' @examples summariseAGE()
#'
#' @import lubridate dplyr tidyr cdmsR
#' @export
#' @return NULL


summariseAGE <- function() {

  # *NO JUVENILE SUMMARY : the Age data currently has only 49 Juvenile records. 
  
  # raw_age <- getDatasetView(datastoreID = 80, cdms_host = cdms_host)
    
  load(file = './data/age_data_07_30_19.rda')
  raw_age <- final_agedata
 
  # Data Processing ----
   
  # Calculate Total European Age
euroage_prep <- raw_age %>%
    filter(!is.na(EuropeanAge)) %>%
    separate(col = EuropeanAge, into = c('EuroFresh', 'EuroSalt'), sep = "\\.", remove = FALSE) %>%
    mutate(EuroFresh = case_when(
      EuroFresh == 'r' ~ 'Unknown',
      EuroFresh != 'r' ~ EuroFresh),
      EuroSalt = gsub('s', '1', EuroSalt),
      EuroSalt_PAD = gsub('', ':', str_pad(EuroSalt, 7, side = 'right', pad = "0")),
      EuroSalt_PAD = substr(EuroSalt_PAD, 2, 14)) %>%
    filter(EuroFresh != 'Unknown') %>%
    mutate(EuroFresh = as.numeric(EuroFresh)) %>%
    separate(col= EuroSalt_PAD, into = c('C1', 'C2', 'C3', 'C4', 'C5', 'C6', 'C7'), sep = ':', remove=FALSE) %>%
    mutate(C1 = as.integer(C1),
           C2 = as.integer(C2),
           C3 = as.integer(C3),
           C4 = as.integer(C4),
           C5 = as.integer(C5),
           C6 = as.integer(C6),
           C7 = as.integer(C7)) %>%
    mutate(EuroSalt = C1 + C2 + C3 + C4 + C5 + C6 + C7,
      Euro_TotalAge = C1 + C2 + C3 + C4 + C5 + C6 + C7 + EuroFresh + 1) %>%
    select(age_unique_id, Euro_TotalAge)


  # Re-join Total Age and finish prep
agedata_mod <- raw_age %>%
  left_join(euroage_prep, by = 'age_unique_id') %>% 
  mutate(SpeciesRun = case_when(
      SpeciesRun == 'S_CHN' ~ 'Spring/Summer Chinook Salmon',
      SpeciesRun == 'S_STH' ~ 'Summer Steelhead',
      SpeciesRun == 'F_CHN' ~ 'Fall Chinook Salmon')#,
      # SpeciesRun == as.factor(SpeciesRun)  # change speciesrun to factor
) %>%
  mutate(CalculatedAge = case_when(    
    !is.na(Euro_TotalAge) ~ Euro_TotalAge,
    is.na(Euro_TotalAge) ~ TotalAge
  )) %>%
  mutate(p_colors = case_when( # assign colors to species (for Plotly later on) !important
    SpeciesRun == 'Fall Chinook Salmon' ~ "#440154FF",
    SpeciesRun == 'Spring/Summer Chinook Salmon' ~ "#21908CFF",
    SpeciesRun == 'Summer Steelhead' ~ "#FDE725FF"))
    # SpeciesRun == 'Fall Chinook Salmon' ~ "blue",
    # SpeciesRun == 'Spring/Summer Chinook Salmon' ~ "firebrick1",
    # SpeciesRun == 'Summer Steelhead' ~ "chartreuse"))

  
  # Adult BestAge Prep ----
adult_df <- agedata_mod %>%
  filter(LifeStage == 'Adult',
         CalculatedAge != -99) %>%
  select(New_UniqueFishId, SpeciesRun, CollectionRepository, AgeDetermination, StreamAge, OceanAge, CalculatedAge, p_colors) %>%
  group_by(New_UniqueFishId, CollectionRepository, AgeDetermination) %>%
  distinct(New_UniqueFishId, .keep_all = TRUE) %>%
  spread(key = AgeDetermination, value = CalculatedAge) %>%
  mutate(BestAge = case_when(
   !is.na(PIT) ~ PIT,
   !is.na(CWT) ~ CWT,
   !is.na(VIE) ~ VIE,
   !is.na(DNA) ~ DNA,
   !is.na(Fin) ~ Fin,
   !is.na(Scale) ~ Scale
  ))

# Summarizing the data outside of Plotly appears to allow me the ability to apply consistent colors to the graphs.
summarized_streamage <- agedata_mod %>%
  filter(StreamAge != -99) %>%
  group_by(CollectionRepository, SpeciesRun, StreamAge, p_colors) %>%
  summarize(Total = n()) 

summarized_oceanage <- agedata_mod %>%
  filter(OceanAge != -99) %>%
  group_by(CollectionRepository, SpeciesRun, OceanAge, p_colors) %>%
  summarize(Total = n())

summarized_totalage <- adult_df %>%
  group_by(CollectionRepository, SpeciesRun, BestAge, p_colors) %>%
  summarize(Total = n()) %>%
  ungroup() %>%
  add_row(CollectionRepository ='FINS', SpeciesRun = 'Fall Chinook Salmon', BestAge = 3, p_colors = "#440154FF", Total = 0)
  

# Graphs----
# Stream Age ----
p_sgs_stream <- plot_ly(data = summarized_streamage %>%
                         filter(SpeciesRun == 'Spring/Summer Chinook Salmon',
                                CollectionRepository == 'NPT-SGS') %>%
                         group_by(CollectionRepository, SpeciesRun, StreamAge),
                       x= ~StreamAge,
                       y= ~Total,
                       type= 'bar',
                       name = 'Spring/Summer Chinook Salmon',
                       marker= list(color = '#21908CFF'),
                       legendgroup = ~SpeciesRun,
                       showlegend = FALSE) %>%
  add_trace(data = summarized_streamage %>%
              filter(SpeciesRun == 'Fall Chinook Salmon',
                     CollectionRepository == 'NPT-SGS') %>%
              group_by(CollectionRepository, SpeciesRun, StreamAge),
            marker= list(color = '#440154FF'),
            name = 'Fall Chinook Salmon') %>%
  add_trace(data = summarized_streamage %>%
              filter(SpeciesRun == 'Summer Steelhead',
                     CollectionRepository == 'NPT-SGS') %>%
              group_by(CollectionRepository, SpeciesRun, StreamAge),
            name = 'Summer Steelhead',
            marker= list(color = '#FDE725FF'),
            name = 'Fall Chinook Salmon')%>%
  layout(legend = list(orientation = 'h', xanchor = 'center', x = 0.5, y = -0.2),
         title = "Stream Age Distributions",
         yaxis = list(title = 'Total'),
         xaxis = list(title = 'Stream Age',
                      range = sort(unique(summarized_streamage$StreamAge)),
                      dtick= 1))


p_weir_stream <- plot_ly(data = summarized_streamage %>%
                          filter(SpeciesRun == 'Spring/Summer Chinook Salmon',
                                 CollectionRepository == 'FINS') %>%
                          group_by(CollectionRepository, SpeciesRun, StreamAge),
                        x= ~StreamAge,
                        y= ~Total,
                        type= 'bar',
                        name = 'Spring/Summer Chinook Salmon',
                        marker= list(color = '#21908CFF'),
                        legendgroup = ~SpeciesRun,
                        showlegend = TRUE
) %>%
  add_trace(data = summarized_streamage %>%
              filter(SpeciesRun == 'Fall Chinook Salmon',
                     CollectionRepository == 'FINS') %>%
              group_by(CollectionRepository, SpeciesRun, StreamAge),
            marker= list(color = '#440154FF'),
            name = 'Fall Chinook Salmon') %>%
  add_trace(data = summarized_streamage %>%
              filter(SpeciesRun == 'Summer Steelhead',
                     CollectionRepository == 'FINS') %>%
              group_by(CollectionRepository, SpeciesRun, StreamAge),
            name = 'Summer Steelhead',
            marker= list(color = '#FDE725FF'),
            name = 'Fall Chinook Salmon') %>%
  layout(legend = list(orientation = 'h', xanchor = 'center', x = 0.5, y = -0.2),
         title = "Stream Age Distributions",
         yaxis = list(title = 'Total'),
         xaxis = list(title = 'Stream Age',
                      range = sort(unique(summarized_streamage$StreamAge)),
                      dtick= 1))

# join stream plots
p_streamage <- subplot(p_sgs_stream, p_weir_stream, nrows = 2, shareX = TRUE)

# Ocean Age ----
p_sgs_ocean <- plot_ly(data = summarized_oceanage %>%
                         filter(SpeciesRun == 'Spring/Summer Chinook Salmon',
                                CollectionRepository == 'NPT-SGS') %>%
                         group_by(CollectionRepository, SpeciesRun, OceanAge),
                       x= ~OceanAge,
                       y= ~Total,
                       type= 'bar',
                       name = 'Spring/Summer Chinook Salmon',
                       marker= list(color = '#21908CFF'),
                       legendgroup = ~SpeciesRun,
                       showlegend = FALSE) %>%
  add_trace(data = summarized_oceanage %>%
              filter(SpeciesRun == 'Fall Chinook Salmon',
                     CollectionRepository == 'NPT-SGS') %>%
              group_by(CollectionRepository, SpeciesRun, OceanAge),
            marker= list(color = '#440154FF'),
            name = 'Fall Chinook Salmon') %>%
  add_trace(data = summarized_oceanage %>%
              filter(SpeciesRun == 'Summer Steelhead',
                     CollectionRepository == 'NPT-SGS') %>%
              group_by(CollectionRepository, SpeciesRun, OceanAge),
            name = 'Summer Steelhead',
            marker= list(color = '#FDE725FF'),
            name = 'Fall Chinook Salmon')%>%
  layout(legend = list(orientation = 'h', xanchor = 'center', x = 0.5, y = -0.2),
         title = "Ocean Age Distributions",
         yaxis = list(title = 'Total'),
         xaxis = list(title = 'Ocean Age',
                      range = sort(unique(summarized_oceanage$OceanAge)),
                      dtick= 1))


p_weir_ocean <- plot_ly(data = summarized_oceanage %>%
                          filter(SpeciesRun == 'Spring/Summer Chinook Salmon',
                                 CollectionRepository == 'FINS') %>%
                          group_by(CollectionRepository, SpeciesRun, OceanAge),
                        x= ~OceanAge,
                        y= ~Total,
                        type= 'bar',
                        name = 'Spring/Summer Chinook Salmon',
                        marker= list(color = '#21908CFF'),
                        legendgroup = ~SpeciesRun,
                        showlegend = TRUE) %>%
  add_trace(data = summarized_oceanage %>%
              filter(SpeciesRun == 'Fall Chinook Salmon',
                     CollectionRepository == 'FINS') %>%
              group_by(CollectionRepository, SpeciesRun, OceanAge),
            marker= list(color = '#440154FF'),
            name = 'Fall Chinook Salmon') %>%
  add_trace(data = summarized_oceanage %>%
              filter(SpeciesRun == 'Summer Steelhead',
                     CollectionRepository == 'FINS') %>%
              group_by(CollectionRepository, SpeciesRun, OceanAge),
            name = 'Summer Steelhead',
            marker= list(color = '#FDE725FF'),
            name = 'Fall Chinook Salmon') %>%
  layout(legend = list(orientation = 'h', xanchor = 'center', x = 0.5, y = -0.2),
         title = "Ocean Age Distributions",
         yaxis = list(title = 'Total'),
         xaxis = list(title = 'Ocean Age',
                      range = sort(unique(summarized_oceanage$OceanAge)),
                      dtick= 1))

# join ocean plots
p_oceanage <- subplot(p_sgs_ocean, p_weir_ocean, nrows = 2, shareX = TRUE)


# Total Age ----
p_sgs_total <- plot_ly(data = summarized_totalage %>%
                         filter(SpeciesRun == 'Spring/Summer Chinook Salmon',
                                CollectionRepository == 'NPT-SGS') %>%
                         group_by(CollectionRepository, SpeciesRun, BestAge),
                       x= ~BestAge,
                       y= ~Total,
                       type= 'bar',
                       name = 'Spring/Summer Chinook Salmon',
                       marker= list(color = '#21908CFF'),
                       legendgroup = ~SpeciesRun,
                       showlegend = FALSE) %>%
  add_trace(data = summarized_totalage %>%
              filter(SpeciesRun == 'Fall Chinook Salmon',
                     CollectionRepository == 'NPT-SGS') %>%
              group_by(CollectionRepository, SpeciesRun, BestAge),
            marker= list(color = '#440154FF'),
            name = 'Fall Chinook Salmon') %>%
  add_trace(data = summarized_totalage %>%
              filter(SpeciesRun == 'Summer Steelhead',
                     CollectionRepository == 'NPT-SGS') %>%
              group_by(CollectionRepository, SpeciesRun, BestAge),
            name = 'Summer Steelhead',
            marker= list(color = '#FDE725FF'),
            name = 'Fall Chinook Salmon')%>%
  layout(legend = list(orientation = 'h', xanchor = 'center', x = 0.5, y = -0.2),
         title = "Total Age Distributions",
         yaxis = list(title = 'Total'),
         xaxis = list(title = 'Total Age',
                      range = sort(unique(summarized_totalage$BestAge)),
                      dtick= 1))


# print(p_sgs_total)

p_weir_total <- plot_ly(data = summarized_totalage %>%
                          filter(SpeciesRun == 'Spring/Summer Chinook Salmon',
                                 CollectionRepository == 'FINS') %>%
                          group_by(CollectionRepository, SpeciesRun, BestAge),
                        x= ~BestAge,
                        y= ~Total,
                        type= 'bar',
                        name = 'Spring/Summer Chinook Salmon',
                        marker= list(color = '#21908CFF'),
                        legendgroup = ~SpeciesRun,
                        showlegend = TRUE) %>%                           
  add_trace(data = summarized_totalage %>%
              filter(SpeciesRun == 'Fall Chinook Salmon',
                     CollectionRepository == 'FINS') %>%
              group_by(CollectionRepository, SpeciesRun, BestAge),
            marker= list(color = '#440154FF'),
            name = 'Fall Chinook Salmon') %>%
  add_trace(data = summarized_totalage %>%
              filter(SpeciesRun == 'Summer Steelhead',
                     CollectionRepository == 'FINS') %>%
              group_by(CollectionRepository, SpeciesRun, BestAge),
            name = 'Summer Steelhead',
            marker= list(color = '#FDE725FF'),
            name = 'Fall Chinook Salmon') %>%
  layout(legend = list(orientation = 'h', xanchor = 'center', x = 0.5, y = -0.2),
         title = "Total Age Distributions",
         yaxis = list(title = 'Total'),
         xaxis = list(title = 'Total Age',
                      range = sort(unique(summarized_totalage$BestAge)),
                      dtick= 1))

# join total plots
p_totalage <- subplot(p_sgs_total, p_weir_total, nrows = 2, shareX = TRUE)


  return(list(p_totalage, p_oceanage, p_streamage))
}