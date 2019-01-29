#' @title getSummaryGraph:
#'
#' @description create simple summary graphs with plotly
#'
#' @param data Survival or Abundance Data (Ex: suv_df / abund_df)
#'
#' @author Tyler Stright
#'
#' @examples getSummaryGraph(data = suv_df, rstfilter = values$RST, yaxis = ~Survival)
#'
#' @import dplyr plotly
#' @export
#' @return NULL
#'
#'

# TESTING
# load('./data/suv_df.rda')
# load('./data/abund_df.rda')
# data <- abund_df
# rstfilter <- 'Imnaha River Rotary Screw Trap'
# yaxis <- ~Survival


# Grab and filter data for one of two tables: Survival OR Abundance
getSummaryGraph <- function(data, rstfilter, yaxis) {

# data filter based on chosen RST (via mapclick)  
tmp_data <- data %>%
  filter(Location == rstfilter,
         Lifestage %in% c('Presmolt', 'Smolt')) %>%
  mutate(cohort = case_when(
    SpeciesRun == 'S_STH' & Origin == 'Natural' & Lifestage == 'Presmolt' ~ 'Natural Steelhead Presmolt',
    SpeciesRun == 'S_STH' & Origin == 'Natural' & Lifestage == 'Smolt' ~ 'Natural Steelhead Smolt',
    SpeciesRun == 'S_STH' & Origin == 'Hatchery' & Lifestage == 'Presmolt' ~ 'Hatchery Steelhead Presmolt',
    SpeciesRun == 'S_STH' & Origin == 'Hatchery' & Lifestage == 'Smolt' ~ 'Hatchery Steelhead Smolt',
    SpeciesRun == 'S_CHN' & Origin == 'Natural' & Lifestage == 'Presmolt' ~ 'Natural Chinook Presmolt',
    SpeciesRun == 'S_CHN' & Origin == 'Natural' & Lifestage == 'Smolt' ~ 'Natural Chinook Smolt',
    SpeciesRun == 'S_CHN' & Origin == 'Hatchery' & Lifestage == 'Presmolt' ~ 'Hatchery Chinook Presmolt',
    SpeciesRun == 'S_CHN' & Origin == 'Hatchery' & Lifestage == 'Smolt' ~ 'Hatchery Chinook Smolt'
  )) %>%
  arrange(MigratoryYear)



p <- plot_ly(data = tmp_data, 
             x = ~MigratoryYear, 
             y = yaxis, 
             type = 'scatter',
             mode = 'lines+markers',
             color = ~cohort,
             colors = viridis_pal(option = "D")(8),
             text = ~paste(cohort)) %>%
  layout(legend = list(x = 0, y = -0.15, orientation = 'h'))

return(p)

}