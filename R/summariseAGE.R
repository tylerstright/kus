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
  
  raw_age <- getDatasetView(datastoreID = 80, cdms_host = cdms_host)
    
  # load(file = './data/age_data_08_06_19.rda')
  # raw_age <- final_agedata
 
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
  select(-C1, -C2, -C3, -C4, -C5, -C6, -C7, -EuroSalt_PAD)


  # Re-join Total Age and finish prep
agedata_mod <- raw_age %>%
  left_join(euroage_prep) %>% 
  mutate(SpeciesRun = case_when(
      SpeciesRun == 'S_CHN' ~ 'Spring/Summer Chinook Salmon',
      SpeciesRun == 'S_STH' ~ 'Summer Steelhead',
      SpeciesRun == 'F_CHN' ~ 'Fall Chinook Salmon')) %>%
  mutate(CalculatedAge = case_when(
    !is.na(Euro_TotalAge) ~ as.double(Euro_TotalAge),
    is.na(Euro_TotalAge) ~ as.double(TotalAge)
  ))

  # Adult BestAge Prep ----
adult_df <- agedata_mod %>%
  filter(Lifestage == 'Adult',
         CalculatedAge != -99) %>%
  select(UniqueFishID, SpeciesRun, Origin, POP_NAME, CollectionRepository, AgeDetermination, StreamAge, OceanAge, CalculatedAge) %>%
  group_by(UniqueFishID, CollectionRepository, AgeDetermination) %>%
  distinct(UniqueFishID, .keep_all = TRUE) %>%
  spread(key = AgeDetermination, value = CalculatedAge) %>%
  mutate(BestAge = case_when(
   !is.na(PIT) ~ PIT,
   !is.na(CWT) ~ CWT,
   !is.na(VIE) ~ VIE,
   !is.na(DNA) ~ DNA,
   !is.na(Fin) ~ Fin,
   !is.na(Scale) ~ Scale
  ))

# Stream Age
streamcounts <- adult_df %>%
  filter(StreamAge!= -99,
         !is.na(StreamAge)) %>%
  group_by(SpeciesRun, CollectionRepository, StreamAge) %>%
  summarize(StreamAgeCount = n()) 

streamtotal <- adult_df %>%
  filter(StreamAge!= -99,
         !is.na(StreamAge)) %>%
  group_by(SpeciesRun, CollectionRepository) %>%
  summarize(StreamAgeTotal = n())

streamage_df <- left_join(streamcounts, streamtotal, by = c('SpeciesRun', 'CollectionRepository')) %>%
  group_by(SpeciesRun, CollectionRepository, StreamAgeCount, StreamAgeTotal) %>%
  mutate(PTOTAL = round((StreamAgeCount/StreamAgeTotal)*100), digits = 0)


# Ocean Age
oceancounts <- adult_df %>%
  filter(OceanAge!= -99,
         !is.na(OceanAge)) %>%
  group_by(SpeciesRun, CollectionRepository, OceanAge) %>%
  summarize(OceanAgeCount = n()) 

oceantotal <- adult_df %>%
  filter(OceanAge!= -99,
         !is.na(OceanAge)) %>%
  group_by(SpeciesRun, CollectionRepository) %>%
  summarize(OceanAgeTotal = n())

oceanage_df <- left_join(oceancounts, oceantotal, by = c('SpeciesRun', 'CollectionRepository')) %>%
  group_by(SpeciesRun, CollectionRepository, OceanAge)%>%
  mutate(PTOTAL = round((OceanAgeCount/OceanAgeTotal)*100), digits = 0)

# Best Age
bestcounts <- adult_df %>%
  filter(BestAge!= -99,
         !is.na(BestAge)) %>%
  group_by(SpeciesRun, CollectionRepository, BestAge) %>%
  summarize(BestAgeCount = n())

besttotal <- adult_df %>%
  filter(BestAge!= -99,
         !is.na(BestAge)) %>%
  group_by(SpeciesRun, CollectionRepository) %>%
  summarize(BestAgeTotal = n())

bestage_df <- left_join(bestcounts, besttotal, by = c('SpeciesRun', 'CollectionRepository')) %>%
  group_by(SpeciesRun, CollectionRepository, BestAge)%>%
  mutate(PTOTAL = round((BestAgeCount/BestAgeTotal)*100), digits = 0)


# Graphs----
  # Stream Age ----
  p_sgs_stream <- plot_ly(legendgroup = ~SpeciesRun,
                          x= ~StreamAge,
                          y= ~PTOTAL,
                          marker= list(line = list(color = 'rgb(0,0,0)', width = 2)),
                          hoverinfo = 'y',
                          showlegend = TRUE) %>%  
  add_bars(data = streamage_df %>%
              filter(SpeciesRun == 'Spring/Summer Chinook Salmon',
                     CollectionRepository == 'NPT-SGS') %>%
              group_by(CollectionRepository, SpeciesRun, StreamAge),
            marker= list(color = '#21908CFF'),
            width = 0.2,
           # hovertemplate = paste('Spring/Summer Chinook Salmon:',
           #                       '<br> %{y:.0f}% Age %{x:.0f}'),
            name = 'Spring/Summer Chinook Salmon') %>%
  add_bars(data = streamage_df %>%
              filter(SpeciesRun == 'Fall Chinook Salmon',
                     CollectionRepository == 'NPT-SGS') %>%
              group_by(CollectionRepository, SpeciesRun, StreamAge),
            marker= list(color = '#440154FF'),
            width = 0.2,
            name = 'Fall Chinook Salmon') %>%
  add_bars(data = streamage_df %>%
              filter(SpeciesRun == 'Summer Steelhead',
                     CollectionRepository == 'NPT-SGS') %>%
              group_by(CollectionRepository, SpeciesRun, StreamAge),
            marker= list(color = '#FDE725FF'),
            width = 0.2,
            name = 'Summer Steelhead') %>%
  layout(legend = list(orientation = 'h', xanchor = 'center', x = 0.5, y = -0.2),
         title = "Freshwater Age Distributions by Collection Method",
         yaxis = list(title = '% of Total (SGS)',
                      ticksuffix = "%"),   # to not format numbers: ticksuffix = '%'
         xaxis = list(title = 'Freshwater Age',
                      range = sort(unique(streamage_df$StreamAge)),
                      dtick= 1))


p_weir_stream <- plot_ly(legendgroup = ~SpeciesRun,
                         x= ~StreamAge,
                         y= ~PTOTAL,
                         name = ~SpeciesRun, 
                         marker= list(line = list(color = 'rgb(0,0,0)', width = 2)),
                         hoverinfo = 'y',
                         showlegend = FALSE) %>%
  add_bars(data = streamage_df %>%
             filter(SpeciesRun == 'Spring/Summer Chinook Salmon',
                    CollectionRepository == 'FINS') %>%
             group_by(CollectionRepository, SpeciesRun, StreamAge),
           marker= list(color = '#21908CFF'),
           width = 0.2,
           name = 'Spring/Summer Chinook Salmon') %>%
  add_bars(data = streamage_df %>%
              filter(SpeciesRun == 'Fall Chinook Salmon',
                     CollectionRepository == 'FINS') %>%
              group_by(CollectionRepository, SpeciesRun, StreamAge),
            marker= list(color = '#440154FF'),
            width = 0.2,
            name = 'Fall Chinook Salmon') %>%
  add_bars(data = streamage_df %>%
              filter(SpeciesRun == 'Summer Steelhead',
                     CollectionRepository == 'FINS') %>%
              group_by(CollectionRepository, SpeciesRun, StreamAge),
            marker= list(color = '#FDE725FF'),
            width = 0.2,
            name = 'Summer Steelhead') %>%
  layout(title = "Freshwater Age Distributions by Collection Method",
         yaxis = list(title = '% of Total (Weir)',
               ticksuffix = "%"),
         xaxis = list(title = 'Freshwater Age',
               range = sort(unique(streamage_df$StreamAge)),
               dtick= 1))

# join stream plots
p_streamage <- subplot(p_sgs_stream, p_weir_stream, nrows = 2, shareX = TRUE, titleY= TRUE)

# print(p_streamage)

  # Ocean Age ----
p_sgs_ocean <- plot_ly(legendgroup = ~SpeciesRun,
                       x= ~OceanAge,
                       y= ~PTOTAL,
                       marker= list(line = list(color = 'rgb(0,0,0)', width = 2)),
                       hoverinfo = 'y',
                       showlegend = TRUE) %>%
  add_bars(data = oceanage_df %>%
             filter(SpeciesRun == 'Spring/Summer Chinook Salmon',
                    CollectionRepository == 'NPT-SGS') %>%
             group_by(CollectionRepository, SpeciesRun, OceanAge),
           marker= list(color = '#21908CFF'),
           width = 0.2,
           name = 'Spring/Summer Chinook Salmon') %>%
  add_bars(data = oceanage_df %>%
             filter(SpeciesRun == 'Fall Chinook Salmon',
                    CollectionRepository == 'NPT-SGS') %>%
             group_by(CollectionRepository, SpeciesRun, OceanAge),
           marker= list(color = '#440154FF'),
           width = 0.2,
           name = 'Fall Chinook Salmon') %>%
  add_bars(data = oceanage_df %>%
             filter(SpeciesRun == 'Summer Steelhead',
                    CollectionRepository == 'NPT-SGS') %>%
             group_by(CollectionRepository, SpeciesRun, OceanAge),
           marker= list(color = '#FDE725FF'),
           width = 0.2,
           name = 'Summer Steelhead') %>%  layout(legend = list(orientation = 'h', xanchor = 'center', x = 0.5, y = -0.2),
         title = "Ocean Age Distributions by Collection Method",
         yaxis = list(title = '% of Total (SGS)',
                      ticksuffix = "%"),
         xaxis = list(title = 'Ocean Age',
                      range = sort(unique(oceanage_df$OceanAge)),
                      dtick= 1))


p_weir_ocean <- plot_ly(legendgroup = ~SpeciesRun,
                        x= ~OceanAge,
                        y= ~PTOTAL,
                        marker= list(line = list(color = 'rgb(0,0,0)', width = 2)),
                        hoverinfo = 'y',
                        showlegend = FALSE) %>%
  add_bars(data = oceanage_df %>%
             filter(SpeciesRun == 'Spring/Summer Chinook Salmon',
                    CollectionRepository == 'FINS') %>%
             group_by(CollectionRepository, SpeciesRun, OceanAge),
           marker= list(color = '#21908CFF'),
           width = 0.2,
           name = 'Spring/Summer Chinook Salmon') %>%
  add_bars(data = oceanage_df %>%
             filter(SpeciesRun == 'Fall Chinook Salmon',
                    CollectionRepository == 'FINS') %>%
             group_by(CollectionRepository, SpeciesRun, OceanAge),
           marker= list(color = '#440154FF'),
           width = 0.2,
           name = 'Fall Chinook Salmon') %>%
  add_bars(data = oceanage_df %>%
             filter(SpeciesRun == 'Summer Steelhead',
                    CollectionRepository == 'FINS') %>%
             group_by(CollectionRepository, SpeciesRun, OceanAge),
           marker= list(color = '#FDE725FF'),
           width = 0.2,
           name = 'Summer Steelhead') %>%
  layout(title = "Ocean Age Distributions by Collection Method",
         yaxis = list(title = '% of Total (Weir)',
                      ticksuffix = "%"),
         xaxis = list(title = 'Ocean Age',
                      range = sort(unique(oceanage_df$OceanAge)),
                      dtick= 1))

# join ocean plots
p_oceanage <- subplot(p_sgs_ocean, p_weir_ocean, nrows = 2, shareX = TRUE, titleY = TRUE)

# print(p_oceanage)

  # Total Age ----
p_sgs_total <- plot_ly(legendgroup = ~SpeciesRun,
                       x= ~BestAge,
                       y= ~PTOTAL,
                       marker= list(line = list(color = 'rgb(0,0,0)', width = 2)),
                       hoverinfo = 'y',
                       showlegend = TRUE) %>%
  add_bars(data = bestage_df %>%
             filter(SpeciesRun == 'Spring/Summer Chinook Salmon',
                    CollectionRepository == 'NPT-SGS') %>%
             group_by(CollectionRepository, SpeciesRun, BestAge),
           marker= list(color = '#21908CFF'),
           width = 0.2,
           name = 'Spring/Summer Chinook Salmon') %>%
  add_bars(data = bestage_df %>%
             filter(SpeciesRun == 'Fall Chinook Salmon',
                    CollectionRepository == 'NPT-SGS') %>%
             group_by(CollectionRepository, SpeciesRun, BestAge),
           marker= list(color = '#440154FF'),
           width = 0.2,
           name = 'Fall Chinook Salmon') %>%
  add_bars(data = bestage_df %>%
             filter(SpeciesRun == 'Summer Steelhead',
                    CollectionRepository == 'NPT-SGS') %>%
             group_by(CollectionRepository, SpeciesRun, BestAge),
           marker= list(color = '#FDE725FF'),
           width = 0.2,
           name = 'Summer Steelhead') %>%  layout(legend = list(orientation = 'h', xanchor = 'center', x = 0.5, y = -0.2),
         title = "Total Age Distributions by Collection Method",
         yaxis = list(title = '% of Total (SGS)',
                      ticksuffix = "%"),
         xaxis = list(title = 'Total Age',
                      range = sort(unique(bestage_df$BestAge)),
                      dtick= 1))


p_weir_total <- plot_ly(legendgroup = ~SpeciesRun,
                        x= ~BestAge,
                        y= ~PTOTAL,
                        marker= list(line = list(color = 'rgb(0,0,0)', width = 2)),
                        hoverinfo = 'y',
                        showlegend = FALSE) %>%                         
  add_bars(data = bestage_df %>%
             filter(SpeciesRun == 'Spring/Summer Chinook Salmon',
                    CollectionRepository == 'FINS') %>%
             group_by(CollectionRepository, SpeciesRun, BestAge),
           marker= list(color = '#21908CFF'),
           width = 0.2,
           name = 'Spring/Summer Chinook Salmon') %>%
  add_bars(data = bestage_df %>%
             filter(SpeciesRun == 'Fall Chinook Salmon',
                    CollectionRepository == 'FINS') %>%
             group_by(CollectionRepository, SpeciesRun, BestAge),
           marker= list(color = '#440154FF'),
           width = 0.2,
           name = 'Fall Chinook Salmon') %>%
  add_bars(data = bestage_df %>%
             filter(SpeciesRun == 'Summer Steelhead',
                    CollectionRepository == 'FINS') %>%
             group_by(CollectionRepository, SpeciesRun, BestAge),
           marker= list(color = '#FDE725FF'),
           width = 0.2,
           name = 'Summer Steelhead') %>%
  layout(#bargap = 0.9, # This spreads apart the X groups.
         title = "Total Age Distributions by Collection Method",
         yaxis = list(title = '% of Total (Weir)',
                      ticksuffix = "%"),
         xaxis = list(title = 'Total Age',
                      range = sort(unique(bestage_df$BestAge)),
                      dtick= 1))

# join total plots
p_totalage <- subplot(p_sgs_total, p_weir_total, nrows = 2, shareX = TRUE, titleY = TRUE)

# print(p_totalage)
# print(p_streamage)
# print(p_oceanage)


# Summarizing for Kus Age Summary Tab ----

# STREAM AGE
stream_sum <- adult_df %>%
  filter(StreamAge != -99) %>%
  group_by(SpeciesRun, Origin, POP_NAME, CollectionRepository, StreamAge) %>%
  summarize(stream_count = n())

stream_sum_total <- adult_df %>%
  filter(StreamAge != -99) %>% #  CollectionRepository != 'PTAGIS'
  group_by(SpeciesRun, Origin, POP_NAME, CollectionRepository) %>%
  summarize(stream_total = n()) %>%
  left_join(stream_sum, by = c('SpeciesRun', 'Origin', 'POP_NAME', 'CollectionRepository')) %>%
  ungroup() %>%
  group_by(SpeciesRun, Origin, POP_NAME, StreamAge) %>%
  mutate(stream_percent = stream_count/stream_total) %>%
  summarise(stream_weighted_mean = weighted.mean(stream_percent, stream_count))

stream_df <- stream_sum_total %>%
  group_by(SpeciesRun, Origin, POP_NAME) %>%
  summarise(wa_sum = sum(stream_weighted_mean)) %>%
  right_join(stream_sum_total, by = c('SpeciesRun', 'Origin', 'POP_NAME')) %>%
  mutate(stream_weighted_mean = stream_weighted_mean/wa_sum)%>%
  select(-wa_sum)
  
# OCEAN AGE
ocean_sum <- adult_df %>%
  filter(OceanAge != -99) %>%
  group_by(SpeciesRun, Origin, POP_NAME, CollectionRepository, OceanAge) %>%
  summarize(ocean_count = n())

ocean_sum_total <- adult_df %>%
  filter(OceanAge != -99) %>%
  group_by(SpeciesRun, Origin, POP_NAME, CollectionRepository) %>%
  summarize(ocean_total = n()) %>%
  left_join(ocean_sum, by = c('SpeciesRun', 'Origin', 'POP_NAME', 'CollectionRepository')) %>%
  ungroup() %>%
  group_by(SpeciesRun, Origin, POP_NAME, OceanAge) %>%
  mutate(ocean_percent = ocean_count/ocean_total) %>%
  summarise(ocean_weighted_mean = weighted.mean(ocean_percent, ocean_count))

ocean_df <- ocean_sum_total %>%
  group_by(SpeciesRun, Origin, POP_NAME) %>%
  summarise(wa_sum = sum(ocean_weighted_mean)) %>%
  right_join(ocean_sum_total, by = c('SpeciesRun', 'Origin', 'POP_NAME')) %>%
  mutate(ocean_weighted_mean = ocean_weighted_mean/wa_sum) %>%
  select(-wa_sum)

# TOTAL AGE (Best = chosen from avaiable sample types)
best_sum <- adult_df %>%
  filter(BestAge != -99) %>%
  group_by(SpeciesRun, Origin, POP_NAME, CollectionRepository, BestAge) %>%
  summarize(best_count = n())

best_sum_total <- adult_df %>%
  filter(BestAge != -99) %>% 
  group_by(SpeciesRun, Origin, POP_NAME, CollectionRepository) %>%
  summarize(best_total = n()) %>%
  left_join(best_sum, by = c('SpeciesRun', 'Origin', 'POP_NAME', 'CollectionRepository')) %>%
  ungroup() %>%
  group_by(SpeciesRun, Origin, POP_NAME, BestAge) %>%
  mutate(best_percent = best_count/best_total) %>%
  summarise(best_weighted_mean = weighted.mean(best_percent, best_count))
  
best_df <- best_sum_total %>%
  group_by(SpeciesRun, Origin, POP_NAME) %>%
  summarise(wa_sum = sum(best_weighted_mean)) %>%
  right_join(best_sum_total, by = c('SpeciesRun', 'Origin', 'POP_NAME')) %>%
  mutate(best_weighted_mean = best_weighted_mean/wa_sum) %>%
  select(-wa_sum)


age_summary_df <- stream_df %>%
  left_join(ocean_df, by = c('SpeciesRun', 'Origin', 'POP_NAME')) %>%
  left_join(best_df, by = c('SpeciesRun', 'Origin', 'POP_NAME')) %>%
  select(SpeciesRun, Origin, POP_NAME, StreamAge, OceanAge, BestAge, stream_weighted_mean, ocean_weighted_mean, best_weighted_mean) %>%
  mutate(t_colors = case_when(
    BestAge == 0 ~ "#440154FF",
    BestAge == 1 ~ "#443A83FF",
    BestAge == 2 ~ "#31688EFF",
    BestAge == 3 ~ "#21908CFF",
    BestAge == 4 ~ "#35B779FF",
    BestAge == 5 ~ "#8FD744FF",
    BestAge == 6 ~ "#FDE725FF"),
    o_colors = case_when(
      OceanAge == 0 ~ "#440154FF",
      OceanAge == 1 ~ "#443A83FF",
      OceanAge == 2 ~ "#31688EFF",
      OceanAge == 3 ~ "#21908CFF",
      OceanAge == 4 ~ "#35B779FF",
      OceanAge == 5 ~ "#8FD744FF",
      OceanAge == 6 ~ "#FDE725FF"),
    s_colors = case_when(
      StreamAge == 0 ~ "#440154FF",
      StreamAge == 1 ~ "#443A83FF",
      StreamAge == 2 ~ "#31688EFF",
      StreamAge == 3 ~ "#21908CFF",
      StreamAge == 4 ~ "#35B779FF",
      StreamAge == 5 ~ "#8FD744FF",
      StreamAge == 6 ~ "#FDE725FF")
    )
  

  return(list(p_totalage, p_oceanage, p_streamage, age_summary_df))
}
