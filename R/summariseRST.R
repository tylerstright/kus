#' @title summariseRST:
#'
#' @description Summarise Rotary Screw Trap data by Trap/Year, combine with survival data.
#'
#' @param streamfilter input$summ_streams
#'
#' @author Tyler Stright
#'
#' @examples summariseRST(streamfilter = input$summrst, rst_data = abund_df, suv_data = suv_df)
#'
#' @import lubridate dplyr tidyr
#' @export
#' @return NULL


summariseRST <- function(rst_data, suv_data, species, startyear, endyear, location) {
  
  tmp_abund <- rst_data %>%
    select(Location, BroodYear, MigratoryYear, SpeciesRun, Origin, Lifestage, Abundance,
           `Abundance SE` = StdError)

  tmp_suv <- suv_data %>%
    select(Location, BroodYear, MigratoryYear, SpeciesRun, Origin, Lifestage,
           `Survival` = Survival,
           `Survival SE` = StdError)

  rst_df <- full_join(tmp_abund, tmp_suv, by = c("Location", "BroodYear", "MigratoryYear", "SpeciesRun", "Origin", "Lifestage")) %>%
    mutate(`Species` = case_when(
      `SpeciesRun` == 'S_CHN' ~ 'Spring/Summer Chinook',
      `SpeciesRun` == 'F_CHN' ~ 'Fall Chinook',
      `SpeciesRun` == 'S_STH' ~ 'Summer Steelhead')) %>%
    filter(Location %in% location) %>%
    filter(Species %in% species) %>%
    filter(MigratoryYear >= startyear) %>%
    filter(MigratoryYear <= endyear) %>%
    select(Location, BroodYear, MigratoryYear, Species, everything(), -SpeciesRun)
  
return(rst_df)
  
}