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
  
  # temporary df
  tmp_abundance <- getDatasetView(datastoreID = 85, cdms_host = cdms_host) %>%
    mutate(SpeciesRun = paste(Run, Species)) %>%
           # Year = MigratoryYear) %>%
    rename(Ab_SE = StdError, Ab_L95 = Lower95, Ab_U95 = Upper95)  # rename common fields

  tmp_survival <- getDatasetView(datastoreID = 86, cdms_host = cdms_host) %>%
    mutate(SpeciesRun = paste(Run, Species)) %>%
           # Year = MigratoryYear) %>%
    rename(Surv_SE = StdError, Surv_L95 = Lower95, Surv_U95 = Upper95) # rename common fields
  
  # base df - will feed into Juvenile Report
  base_df <- full_join(tmp_abundance, tmp_survival, by = c('ESU_DPS', 'MPG', 'POP_NAME', 'TRT_POPID', 
                                                           'SpeciesRun', 'Species', 'Run', 'StreamName', 
                                                           'TribToName', 'LocationLabel', 'Origin', 'BroodYear', 
                                                           'MigratoryYear', 'Lifestage', 'Year')) %>%
    mutate(Ab_SE = round(Ab_SE, 0),
           Equivalents = round(Abundance*Survival, digits = 0),
           Survival = round(Survival, 2),
           Surv_SE = round(Surv_SE, 2)) %>%
    arrange(Year)

  # Dataframe for RST Summary Tab :
  RSTsummary_df <- base_df %>%
    mutate(Ab_L95_errorbar = Abundance-Ab_L95,
           Ab_U95_errorbar = Ab_U95-Abundance,
           Surv_L95_errorbar = Survival-Surv_L95,
           Surv_U95_errorbar = Surv_U95-Survival) %>%
    select(POP_NAME, LocationLabel, SpeciesRun, Origin, BroodYear, MigratoryYear, Lifestage,
           Abundance, Ab_SE, Ab_L95, Ab_U95, Ab_L95_errorbar, Ab_U95_errorbar, ReleaseType, ReleaseGroup,
           SurvivalTo, Survival, Surv_SE, Surv_L95, Surv_U95, Surv_L95_errorbar, Surv_U95_errorbar, Equivalents) %>%
    filter(Lifestage == 'Smolt')

  
  # Dataframe for Custom Queries Tab :
  RST_customq_df <- base_df %>%
    select(POP_NAME, StreamName, LocationLabel, SpeciesRun, Origin, BroodYear, MigratoryYear, Lifestage,
           Abundance, Ab_SE, Ab_L95, Ab_U95, ReleaseType, ReleaseGroup, 
           SurvivalTo, Survival, Surv_SE, Surv_L95, Surv_U95, Equivalents)
            # Select the fields we want to appear in the field selection input.


return(list(RSTsummary_df, RST_customq_df, base_df))
  
}