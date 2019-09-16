#' @title summariseRST
#'
#' @description Combine CDMS RST Abundance and Survival Summaries, calculate Equivalents.
#'
#' @author Tyler Stright
#'
#' @examples summariseRST() 
#'
#' @import tidyverse
#' @export
#' @return NULL


summariseRST <- function() {
  
  tmp_abundance <- getDatasetView(datastoreID = 85, cdms_host = cdms_host) %>%
    mutate(SpeciesRun = paste(Run, Species),
           Year = MigratoryYear) %>%
    rename(Ab_SE = StdError, Ab_L95 = Lower95, Ab_U95 = Upper95) %>% # rename common fields
    select(Year, POP_NAME, SpeciesRun, Origin, BroodYear, MigratoryYear, Lifestage, 
           Abundance, Ab_SE, Ab_L95, Ab_U95)
  
  tmp_survival <- getDatasetView(datastoreID = 86, cdms_host = cdms_host) %>%
    mutate(SpeciesRun = paste(Run, Species),
           Year = MigratoryYear) %>%
    rename(Surv_SE = StdError, Surv_L95 = Lower95, Surv_U95 = Upper95) %>% # rename common fields
    select(Year, POP_NAME, SpeciesRun, Origin, BroodYear, MigratoryYear, Lifestage, 
           ReleaseType, SurvivalTo, Survival, Surv_SE) # Surv_L95, Surv_U95
  
  RSTsummary_df <- tmp_abundance %>%
    left_join(tmp_survival, by = c('POP_NAME', 'SpeciesRun', 'Origin', 'BroodYear', 'MigratoryYear', 'Lifestage', 'Year')) %>%
    mutate(Ab_SE = round(Ab_SE, 0),
           Equivalents = round(Abundance*Survival, digits = 0),
           Survival = round(Survival, 2),
           Surv_SE = round(Surv_SE, 2)) %>%
    arrange(Year)
    

return(RSTsummary_df)
  
}