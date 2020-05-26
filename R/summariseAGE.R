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


summariseAGE <- function(age_data) {

  # *NO JUVENILE SUMMARY : the Age data currently has only 49 Juvenile records. 
 
  # Data Processing ----
   
  # Calculate Total European Age
euroage_prep <- age_data %>%
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
agedata_mod <- age_data %>%
  left_join(euroage_prep) %>% 
  mutate(SpeciesRun = case_when(
      SpeciesRun == 'S_CHN' ~ 'Spring/summer Chinook salmon',
      SpeciesRun == 'S_STH' ~ 'Summer Steelhead',
      SpeciesRun == 'F_CHN' ~ 'Fall Chinook salmon')) %>%
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
  summarise(StreamAge_Wmean = weighted.mean(stream_percent, stream_count))

stream_df <- stream_sum_total %>%
  group_by(SpeciesRun, Origin, POP_NAME) %>%
  summarise(wa_sum = sum(StreamAge_Wmean)) %>%
  right_join(stream_sum_total, by = c('SpeciesRun', 'Origin', 'POP_NAME')) %>%
  mutate(StreamAge_Wmean = StreamAge_Wmean/wa_sum)%>%
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
  summarise(OceanAge_Wmean = weighted.mean(ocean_percent, ocean_count))

ocean_df <- ocean_sum_total %>%
  group_by(SpeciesRun, Origin, POP_NAME) %>%
  summarise(wa_sum = sum(OceanAge_Wmean)) %>%
  right_join(ocean_sum_total, by = c('SpeciesRun', 'Origin', 'POP_NAME')) %>%
  mutate(OceanAge_Wmean = OceanAge_Wmean/wa_sum) %>%
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
  summarise(BestAge_Wmean = weighted.mean(best_percent, best_count))
  
best_df <- best_sum_total %>%
  group_by(SpeciesRun, Origin, POP_NAME) %>%
  summarise(wa_sum = sum(BestAge_Wmean)) %>%
  right_join(best_sum_total, by = c('SpeciesRun', 'Origin', 'POP_NAME')) %>%
  mutate(BestAge_Wmean = BestAge_Wmean/wa_sum) %>%
  select(-wa_sum)


age_summary_df <- stream_df %>%
  left_join(ocean_df, by = c('SpeciesRun', 'Origin', 'POP_NAME')) %>%
  left_join(best_df, by = c('SpeciesRun', 'Origin', 'POP_NAME')) %>%
  select(SpeciesRun, Origin, POP_NAME, StreamAge, OceanAge, BestAge, StreamAge_Wmean, OceanAge_Wmean, BestAge_Wmean) %>%
  mutate(b_colors = case_when(
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
    ) %>%
  mutate_at(vars(StreamAge, OceanAge, BestAge), funs(as.character))
  
# Stream Age Summary
StreamAge_df <- age_summary_df %>%
  group_by(SpeciesRun, Origin, POP_NAME, StreamAge, StreamAge_Wmean, colors = s_colors) %>%
  distinct(StreamAge) %>%
  arrange(StreamAge)

# Ocean Age Summary 
OceanAge_df <- age_summary_df %>%
  group_by(SpeciesRun, Origin, POP_NAME, OceanAge, OceanAge_Wmean, colors = o_colors) %>%
  distinct(OceanAge) %>%
  arrange(OceanAge)

# Total Age Summary
BestAge_df <- age_summary_df %>%
  group_by(SpeciesRun, Origin, POP_NAME, BestAge, BestAge_Wmean, colors = b_colors) %>%
  distinct(BestAge) %>%
  arrange(BestAge)



  return(list(age_summary_df, BestAge_df, OceanAge_df, StreamAge_df))
}
