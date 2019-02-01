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
getSummaryGraph <- function(data, metric) {

# data filter based on chosen RST (via mapclick)  
tmp_data <- data %>%
  mutate(cohort = paste0(Origin, ' Origin ',Species)) %>%
  arrange(MigratoryYear)
  
  if(metric == ~Abundance){
    df <- tmp_data %>% filter(Lifestage == 'Smolt')
  }
  
  if(metric == ~Survival){
    df <- tmp_data %>% filter(Lifestage == 'Smolt')
  }
  
    p <- plot_ly(data = df, 
                 x = ~MigratoryYear, 
                 y = metric, 
                 type = 'scatter',
                 mode = 'lines+markers',
                 color = ~cohort,
                 colors = viridis_pal(option = "D")(8),
                 text = ~paste(cohort)) %>%
      layout(legend = list(x = 0, y = -0.15, orientation = 'h'))
  
return(p)

}