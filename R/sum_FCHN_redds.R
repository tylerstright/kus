#' @title sum_FCHN_redds
#'
#' @description Summarise & Graph AGE data by POPULATION
#'
#' @author Tyler Stright
#'
#' @examples sum_FCHN_redds()
#'
#' @import lubridate dplyr tidyr cdmsR
#' @export
#' @return NULL


sum_FCHN_redds <- function() {
  
  # Get F_CHN Redd data from CDMS
  redd_df <- getDatasetView(datastoreID = 78, cdms_host = cdms_host)  
  
  redd_df2 <- redd_df %>%  
    filter(WPTType == 'New Redd',
           Species == 'Chinook salmon',
           Run == 'Fall',
           SurveyMethod == 'Helicopter') %>%      
    mutate(SurveyDate = ymd(gsub(pattern = 'T00:00:00', replacement = '', SurveyDate)),
           SurveyYear = year(SurveyDate),
           RKM = as.numeric(str_extract(WPTName, "\\d+\\.*\\d*")),
           start_km = floor(RKM),
           end_km = ceiling(RKM),
           end_km = if_else(end_km == start_km, end_km +1, end_km)) %>%
    group_by(SurveyYear, SurveyDate, StreamName, Pass, start_km, end_km, NewRedds) %>%
    distinct(NewRedds, .keep_all = TRUE) #%>%
        # select(SurveyDate, ESU_DPS, MPG, POP_NAME, StreamName, Species, Run, Pass, StartSurvey, EndSurvey, NewRedds, WPTName)
  
  fchn_redds_df <- redd_df2 %>%
    group_by(SurveyYear, StreamName, start_km, end_km) %>%
    summarise(redd_count = sum(Count)) %>%
    spread(key=SurveyYear, value = redd_count) 
    
  return(fchn_redds_df)
}
