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


summariseRST <- function(rstfilter, rst_data, suv_data) {
  
  tmp_abund <- rst_data %>%
    select(Location, BroodYear, MigratoryYear, SpeciesRun, Origin, Lifestage, Abundance, StdError) %>%
    rename(`Abundance SE` = StdError)

  tmp_suv <- suv_data %>%
    select(Location, BroodYear, MigratoryYear, SpeciesRun, Origin, Lifestage, Survival, StdError) %>%
    mutate(`Survival to LGR (%)` = 100*Survival,
           `Survival SE (%)` = 100*StdError) %>%
    select(-Survival, -StdError)
  
  rst_df <- left_join(tmp_abund, tmp_suv, by = c("Location", "BroodYear", "MigratoryYear", "SpeciesRun", "Origin", "Lifestage")) %>%
    mutate(`Species` = case_when(
      `SpeciesRun` == 'S_CHN' ~ 'Spring/Summer Chinook',
      `SpeciesRun` == 'S_STH' ~ 'Summer Steelhead')) %>%
    filter(Location == rstfilter) %>%
    select(Location, BroodYear, MigratoryYear, Species, everything(), -SpeciesRun)
  
return(rst_df)
  
}